# simulation_plotting.R - ploting for the simulation results from Julia
#
# Copyright (C) 2022 Kianté Fernandez, <kiantefernan@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Record of Revisions
#
# Date            Programmers                         Descriptions of Change
# ====         ================                       ======================
# 12/07/22      Kianté  Fernandez                     Wrote intial code
# 27/07/22      Kianté  Fernandez                     Fixed fixation histogram
# 28/07/22      Kianté  Fernandez                     Added relative difference plot
# 29/07/22      Kianté  Fernandez                     Added first fixation plot
# 30/07/22      Kianté  Fernandez                     Added facet_wrap plot version


library(here)
library(ggplot2)
library(readr)
library(tidyr)
library(magrittr)
library(stringr)
library(tidyverse)
library(patchwork)

filename <- here("simulation_results")

load_sim_data <- function(filename) {
  # loads the simulation datasets into a list where each element is a data set
  temp <- list.files(path = filename, pattern = "*.csv", full.names = T)
  datasets <- vector(mode = "list", length(temp))
  for (idx in 1:length(temp)) {
    datasets[[idx]] <- read_csv(temp[[idx]], show_col_types = F)
    datasets[[idx]]$k <- idx
  }
  return(datasets)
}

dats <- load_sim_data(filename)
full_df <- do.call(rbind, dats) # for facet wrap graphing

# res_subset_size_1 <- read_csv(here("simulation_results", "res_subset_size_1.csv"))

# model fixation refers to a continuous sequence of samples taken from one item.
# each fixation is == 100 ms. So 1,1,2.  is 200 ms on item 1 & 100ms on item two

# color for varying levels of K
plt_colors <- c("deepskyblue1", "darkgoldenrod2", "forestgreen", "firebrick1", "darkorchid3")
# preallocate for plot structures
plot1 <- vector(mode = "list", length = length(dats))
plot2 <- vector(mode = "list", length = length(dats))
plot3 <- vector(mode = "list", length = length(dats))
plot4 <- vector(mode = "list", length = length(dats))
plot5 <- vector(mode = "list", length = length(dats))

for (plt_idx in seq_len(length(dats))) {
  data_set <- dats[[plt_idx]] # for each level of k...

  # munge the data to get it into a format for plotting
  mung_df <- data_set %>%
    mutate(
      trial_idx = seq_len(nrow(data_set)),
      fixations = gsub("\\[|\\]", "", fixations),
      choice = gsub("\\[|\\]", "", choice)
    ) %>%
    separate_rows(fixations, sep = ",") %>%
    mutate(
      fixations = gsub(" ", "", fixations, fixed = TRUE),
      fixations = as.numeric(fixations)
    ) %>%
    group_by(trial_idx) %>%
    mutate(fixation_idx = seq_len(n()))

  # Kernel density estimation for the distribution of total fixation time
  mung_df %>%
    group_by(trial_idx, fixations) %>%
    tally() %>%
    mutate(fixation_duration = n * 100, .keep = "unused") %>%
    group_by(trial_idx) %>%
    summarise(rt = sum(fixation_duration)) %>%
    ggplot(aes(rt, y = ..density..)) +
    geom_histogram(fill = "white", color = "black", binwidth = 300) +
    geom_density(size = 1, color = plt_colors[[plt_idx]]) +
    labs(
      x = "Total fixation time [ms]",
      y = "Density",
      title = "Kernel density estimation for the distribution of total fixation time",
      subtitle = paste0("Subset size: ", plt_idx)
    ) +
    theme_classic() +
    theme(legend.position = "none") -> plot1[[plt_idx]]

  # Histogram of number of fixations in a trial
  ## recall that a sequence of the same item is a single fixation, needs to me counted that way
  mung_df %>%
    group_by(trial_idx) %>%
    mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
    slice_max(fixation_num) %>%
    ggplot(aes(fixation_num, y = ..density..)) +
    geom_histogram(fill = "white", color = "black", binwidth = 5) +
    geom_density(size = 1, color = plt_colors[[plt_idx]]) +
    labs(
      x = "Number of fixations",
      y = "Proportion of trials",
      title = "Histogram of number of fixations in a trial",
      subtitle = paste0("Subset size: ", plt_idx)
    ) +
    theme_classic() +
    theme(legend.position = "none") -> plot2[[plt_idx]]

  # Total fixation time as a function of the mean of all the item ratings (overall value)
  mung_df %>%
    group_by(trial_idx, fixations, avg_value_idx) %>%
    tally() %>%
    mutate(fixation_duration = n * 100, .keep = "unused") %>%
    group_by(avg_value_idx, trial_idx) %>%
    summarise(rt = sum(fixation_duration)) %>%
    ggplot(aes(avg_value_idx, rt)) +
    stat_summary(fun.data = mean_se, geom = "errorbar") +
    stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
    stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
    labs(
      x = "Mean item rating",
      y = "Total fixation time [ms]",
      title = "Total fixation time as a function of the mean of all the item ratings (overall value)",
      subtitle = paste0("Subset size: ", plt_idx)
    ) +
    theme_classic() +
    theme(legend.position = "none") -> plot3[[plt_idx]]

  # Total fixation time as a function of the relative rating of the highest rated item
  ## get fixation time
  total_fixation_time <- mung_df %>%
    group_by(trial_idx, fixations, avg_value_idx) %>%
    tally() %>%
    mutate(fixation_duration = n * 100, .keep = "unused") %>%
    group_by(trial_idx, avg_value_idx) %>%
    summarise(rt = sum(fixation_duration))

  data_set %>%
    mutate(
      trial_idx = seq_len(nrow(data_set)),
      item_value = gsub("\\[|\\]", "", ss),
      choice = gsub("\\[|\\]", "", choice), .keep = "unused"
    ) %>%
    separate_rows(item_value, sep = ",") %>%
    mutate(
      item_value = gsub(" ", "", item_value, fixed = TRUE),
      item_value = as.numeric(item_value)
    ) %>%
    group_by(trial_idx) %>%
    mutate(
      value_idx = seq_len(n()),
      max_value = max(item_value), # get highest rated item
      mean_other = mean(subset(.$item_value, item_value != max_value)), # get the mean of the other items (needs subset still)
      relative_rating = max_value - mean_other
    ) %>%
    select(trial_idx, relative_rating) %>%
    distinct() %>%
    left_join(total_fixation_time, by = "trial_idx") %>%
    mutate(relative_rating = round(relative_rating)) %>% # to create bins for plotting
    ggplot(aes(relative_rating, rt)) +
    stat_summary(fun.data = mean_se, geom = "errorbar") +
    stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
    stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
    labs(
      x = "Best rating - mean other rating",
      y = "Total fixation time [ms]",
      title = "Total fixation time as a function of the relative rating of the highest rated item",
      subtitle = paste0("Subset size: ", plt_idx)
    ) +
    theme_classic() +
    theme(legend.position = "none") -> plot4[[plt_idx]]
  # First fixation duration as a function of the rating of the first-fixated item
  # get first fixation
  first_fixation_duration <- data_set %>%
    mutate(
      trial_idx = seq_len(nrow(data_set)),
      fixations = gsub("\\[|\\]", "", fixations),
      choice = gsub("\\[|\\]", "", choice)
    ) %>%
    separate_rows(fixations, sep = ",") %>%
    mutate(
      fixations = gsub(" ", "", fixations, fixed = TRUE),
      fixations = as.numeric(fixations)
    ) %>%
    group_by(trial_idx) %>%
    mutate(timestep_idx = seq_len(n())) %>%
    mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
    filter(fixation_num == 0) %>%
    group_by(trial_idx, fixations, avg_value_idx) %>%
    tally() %>%
    mutate(fixation_duration = n * 100, .keep = "unused")

  data_set %>%
    mutate(
      trial_idx = seq_len(nrow(data_set)),
      item_value = gsub("\\[|\\]", "", ss),
    ) %>%
    separate_rows(item_value, sep = ",") %>%
    mutate(
      item_value = gsub(" ", "", item_value, fixed = TRUE),
      item_value = as.numeric(item_value)
    ) %>%
    group_by(trial_idx) %>%
    mutate(value_idx = seq_len(n())) %>%
    select(trial_idx, item_value, value_idx) %>%
    left_join(first_fixation_duration, by = "trial_idx") %>%
    group_by(trial_idx) %>%
    filter(fixations == value_idx) %>%
    mutate(item_value = round(item_value)) %>% # to create bins for plotting
    ggplot(aes(item_value, fixation_duration)) +
    stat_summary(fun.data = mean_se, geom = "errorbar") +
    stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
    stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
    labs(
      x = "First fixated item rating",
      y = "First fixation duration [ms]",
      title = "First fixation duration as a function of the rating of the first-fixated item",
      subtitle = paste0("Subset size: ", plt_idx)
    ) +
    theme_classic() +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(-2, 0, 2, 4)) -> plot5[[plt_idx]]
}

create_subplot <- function(plots, filename = NULL) {
  # create subplots plot for each subset size
  print((plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]))
  if (!is.null(filename)) {
    ggsave(here("figures", filename), dpi = 300)
  }
}
create_subplot(plot1, "density_total_fixation_time.png")
create_subplot(plot2, "histogram_number_of_fixations.png")
create_subplot(plot3, "fixation_time_mean_value.png")
create_subplot(plot4, "fixation_time_relative_value.png")
create_subplot(plot5, "first_fixation_first_value.png")

## %######################################################%##
#                                                          #
####            long format way using facet wrap        ####
#                                                          #
## %######################################################%##

mung_df <- full_df %>%
  group_by(k) %>%
  mutate(
    trial_idx = seq_len(n()),
    fixations = gsub("\\[|\\]", "", fixations),
    choice = gsub("\\[|\\]", "", choice)
  ) %>%
  separate_rows(fixations, sep = ",") %>%
  mutate(
    fixations = gsub(" ", "", fixations, fixed = TRUE),
    fixations = as.numeric(fixations)
  ) %>%
  group_by(k, trial_idx) %>%
  mutate(fixation_idx = seq_len(n()))

# Kernel density estimation for the distribution of total fixation time
mung_df %>%
  group_by(k, trial_idx, fixations) %>%
  tally() %>%
  mutate(fixation_duration = n * 100, .keep = "unused") %>%
  group_by(k, trial_idx) %>%
  summarise(rt = sum(fixation_duration)) %>%
  ggplot(aes(rt, y = ..density.., color = factor(k), group = factor(k))) +
  geom_histogram(fill = "white", color = "black", binwidth = 300) +
  geom_density(size = 1) +
  labs(
    x = "Total fixation time [ms]",
    y = "Density",
    title = "Kernel density estimation for the distribution of total fixation time"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~k, labeller = "label_both", scales = "fixed") +
  scale_color_brewer(palette = "Set1")

# Histogram of number of fixations in a trial
mung_df %>%
  group_by(k, trial_idx) %>%
  mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
  slice_max(fixation_num) %>%
  ggplot(aes(fixation_num, y = ..density.., color = factor(k), group = factor(k))) +
  geom_histogram(fill = "white", color = "black", binwidth = 5) +
  geom_density(size = 1) +
  labs(
    x = "Number of fixations",
    y = "Proportion of trials",
    title = "Histogram of number of fixations in a trial"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~k, labeller = "label_both", scales = "fixed") +
  scale_color_brewer(palette = "Set1")

# Total fixation time as a function of the mean of all the item ratings (overall value)
mung_df %>%
  group_by(k, trial_idx, fixations, avg_value_idx) %>%
  tally() %>%
  mutate(fixation_duration = n * 100, .keep = "unused") %>%
  group_by(k, avg_value_idx, trial_idx) %>%
  summarise(rt = sum(fixation_duration)) %>%
  ggplot(aes(avg_value_idx, rt, color = factor(k), group = factor(k))) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun = mean, geom = "line", size = .7) +
  labs(
    x = "Mean item rating",
    y = "Total fixation time [ms]",
    title = "Total fixation time as a function of the mean of all the item ratings (overall value)"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~k, labeller = "label_both", scales = "fixed")

# Total fixation time as a function of the relative rating of the highest rated item
# NOTE: does not work yet, mutate that uses subset is computationally intensive
# total_fixation_time <- mung_df %>% group_by(k, trial_idx, fixations, avg_value_idx) %>% tally() %>%
#   mutate(fixation_duration = n * 100, .keep = "unused") %>%
#   group_by(k, trial_idx, avg_value_idx) %>%
#   summarise(rt = sum(fixation_duration))
#
# full_df %>%
#   group_by(k) %>%
#   mutate(
#     trial_idx = seq_len(n()),
#     item_value = gsub("\\[|\\]", "", ss),
#     choice = gsub("\\[|\\]", "", choice), .keep = "unused"
#   ) %>%
#   separate_rows(item_value, sep = ",") %>%
#   mutate(
#     item_value = gsub(" ", "", item_value, fixed = TRUE),
#     item_value = as.numeric(item_value)
#   ) %>%
#   group_by(k,trial_idx) %>%
#   mutate(value_idx = seq_len(n()),
#          max_value = max(item_value), #get highest rated item
#          mean_other = mean(subset(.$item_value, item_value != max_value)), #get the mean of the other items
#          relative_rating = max_value - mean_other) %>%
#   select(k, trial_idx, relative_rating) %>% distinct() %>%
#   left_join(total_fixation_time, by = c("k","trial_idx")) %>%
#   mutate(relative_rating = round(relative_rating)) %>% #to create bins for plotting
#   ggplot(aes(relative_rating, rt, color = factor(k), group = factor(k))) +
#   stat_summary(fun.data = mean_se, geom = "errorbar") +
#   stat_summary(fun.data = mean_se, geom = "point") +
#   stat_summary(fun = mean, geom = "line", size = .7)+
#   labs(
#     x = "Best rating - mean other rating",
#     y = "Total fixation time [ms]",
#     title = "Total fixation time as a function of the relative rating of the highest rated item",
#   ) + theme_classic() + theme(legend.position="none") +scale_color_brewer(palette="Set1")+
#   facet_wrap(~k, labeller = "label_both", scales = "fixed")

# First fixation duration as a function of the rating of the first-fixated item
first_fixation_duration <- full_df %>%
  group_by(k) %>%
  mutate(
    trial_idx = seq_len(n()),
    fixations = gsub("\\[|\\]", "", fixations),
    choice = gsub("\\[|\\]", "", choice)
  ) %>%
  separate_rows(fixations, sep = ",") %>%
  mutate(
    fixations = gsub(" ", "", fixations, fixed = TRUE),
    fixations = as.numeric(fixations)
  ) %>%
  group_by(k, trial_idx) %>%
  mutate(timestep_idx = seq_len(n())) %>%
  mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
  filter(fixation_num == 0) %>%
  group_by(k, trial_idx, fixations, avg_value_idx) %>%
  tally() %>%
  mutate(fixation_duration = n * 100, .keep = "unused")

full_df %>%
  group_by(k) %>%
  mutate(
    trial_idx = seq_len(n()),
    item_value = gsub("\\[|\\]", "", ss),
  ) %>%
  separate_rows(item_value, sep = ",") %>%
  mutate(
    item_value = gsub(" ", "", item_value, fixed = TRUE),
    item_value = as.numeric(item_value)
  ) %>%
  group_by(k, trial_idx) %>%
  mutate(value_idx = seq_len(n())) %>%
  select(k, trial_idx, item_value, value_idx) %>%
  left_join(first_fixation_duration, by = c("trial_idx", "k")) %>%
  group_by(k, trial_idx) %>%
  filter(fixations == value_idx) %>%
  mutate(item_value = round(item_value)) %>% # to create bins for plotting
  ggplot(aes(item_value, fixation_duration, color = factor(k), group = factor(k))) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun = mean, geom = "line", size = .7) +
  labs(
    x = "First fixated item rating",
    y = "First fixation duration [ms]",
    title = "First fixation duration as a function of the rating of the first-fixated item"
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = c(-2, 0, 2, 4)) +
  facet_wrap(~k, labeller = "label_both", scales = "fixed")


# Probability of choosing the first-seen item as a function of the first-fixation duration
