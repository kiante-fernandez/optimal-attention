# preallocate for plot structures
# plot1 <- vector(mode = "list", length = length(dats))
# plot2 <- vector(mode = "list", length = length(dats))
# plot3 <- vector(mode = "list", length = length(dats))
# plot4 <- vector(mode = "list", length = length(dats))
# plot5 <- vector(mode = "list", length = length(dats))
#
# for (plt_idx in seq_len(length(dats))) {
#   data_set <- dats[[plt_idx]] # for each level of k...
#
#   # munge the data to get it into a format for plotting
#   mung_df <- data_set %>%
#     mutate(
#       trial_idx = seq_len(nrow(data_set)),
#       fixations = gsub("\\[|\\]", "", fixations),
#       choice = gsub("\\[|\\]", "", choice)
#     ) %>%
#     separate_rows(fixations, sep = ",") %>%
#     mutate(
#       fixations = gsub(" ", "", fixations, fixed = TRUE),
#       fixations = as.numeric(fixations)
#     ) %>%
#     group_by(trial_idx) %>%
#     mutate(fixation_idx = seq_len(n()))
#
#   # Kernel density estimation for the distribution of total fixation time
#   mung_df %>%
#     group_by(trial_idx, fixations) %>%
#     tally() %>%
#     mutate(fixation_duration = n * 100, .keep = "unused") %>%
#     group_by(trial_idx) %>%
#     summarise(rt = sum(fixation_duration)) %>%
#     ggplot(aes(rt, y = ..density..)) +
#     geom_histogram(fill = "white", color = "black", binwidth = 300) +
#     geom_density(size = 1, color = plt_colors[[plt_idx]]) +
#     labs(
#       x = "Total fixation time [ms]",
#       y = "Density",
#       title = "Kernel density estimation for the distribution of total fixation time",
#       subtitle = paste0("Subset size: ", plt_idx)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none") -> plot1[[plt_idx]]
#
#   # Histogram of number of fixations in a trial
#   mung_df %>%
#     group_by(trial_idx) %>%
#     mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
#     slice_max(fixation_num) %>%
#     ggplot(aes(fixation_num, y = ..density..)) +
#     geom_histogram(fill = "white", color = "black", binwidth = 5) +
#     geom_density(size = 1, color = plt_colors[[plt_idx]]) +
#     labs(
#       x = "Number of fixations",
#       y = "Proportion of trials",
#       title = "Histogram of number of fixations in a trial",
#       subtitle = paste0("Subset size: ", plt_idx)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none") -> plot2[[plt_idx]]
#
#   # Total fixation time as a function of the mean of all the item ratings (overall value)
#   mung_df %>%
#     group_by(trial_idx, fixations, avg_value_idx) %>%
#     tally() %>%
#     mutate(fixation_duration = n * 100, .keep = "unused") %>%
#     group_by(avg_value_idx, trial_idx) %>%
#     summarise(rt = sum(fixation_duration)) %>%
#     ggplot(aes(avg_value_idx, rt)) +
#     stat_summary(fun.data = mean_se, geom = "errorbar") +
#     stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
#     stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
#     labs(
#       x = "Mean item rating",
#       y = "Total fixation time [ms]",
#       title = "Total fixation time as a function of the mean of all the item ratings (overall value)",
#       subtitle = paste0("Subset size: ", plt_idx)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none") -> plot3[[plt_idx]]
#
#   # Total fixation time as a function of the relative rating of the highest rated item
#   ## get fixation time
#   total_fixation_time <- mung_df %>%
#     group_by(trial_idx, fixations, avg_value_idx) %>%
#     tally() %>%
#     mutate(fixation_duration = n * 100, .keep = "unused") %>%
#     group_by(trial_idx, avg_value_idx) %>%
#     summarise(rt = sum(fixation_duration))
#
#   data_set %>%
#     mutate(
#       trial_idx = seq_len(nrow(data_set)),
#       item_value = gsub("\\[|\\]", "", ss),
#       choice = gsub("\\[|\\]", "", choice), .keep = "unused"
#     ) %>%
#     separate_rows(item_value, sep = ",") %>%
#     mutate(
#       item_value = gsub(" ", "", item_value, fixed = TRUE),
#       item_value = as.numeric(item_value)
#     ) %>%
#     group_by(trial_idx) %>%
#     mutate(
#       value_idx = seq_len(n()),
#       max_value = max(item_value), # get highest rated item
#       mean_other = mean(subset(.$item_value, item_value != max_value)), # get the mean of the other items (needs subset still)
#       relative_rating = max_value - mean_other
#     ) %>%
#     select(trial_idx, relative_rating) %>%
#     distinct() %>%
#     left_join(total_fixation_time, by = "trial_idx") %>%
#     mutate(relative_rating = round(relative_rating)) %>% # to create bins for plotting
#     ggplot(aes(relative_rating, rt)) +
#     stat_summary(fun.data = mean_se, geom = "errorbar") +
#     stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
#     stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
#     labs(
#       x = "Best rating - mean other rating",
#       y = "Total fixation time [ms]",
#       title = "Total fixation time as a function of the relative rating of the highest rated item",
#       subtitle = paste0("Subset size: ", plt_idx)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none") -> plot4[[plt_idx]]
#   # First fixation duration as a function of the rating of the first-fixated item
#   # get first fixation
#   first_fixation_duration <- data_set %>%
#     mutate(
#       trial_idx = seq_len(nrow(data_set)),
#       fixations = gsub("\\[|\\]", "", fixations),
#       choice = gsub("\\[|\\]", "", choice)
#     ) %>%
#     separate_rows(fixations, sep = ",") %>%
#     mutate(
#       fixations = gsub(" ", "", fixations, fixed = TRUE),
#       fixations = as.numeric(fixations)
#     ) %>%
#     group_by(trial_idx) %>%
#     mutate(timestep_idx = seq_len(n())) %>%
#     mutate(fixation_num = cumsum(c(0, as.numeric(diff(fixations)) != 0))) %>%
#     filter(fixation_num == 0) %>%
#     group_by(trial_idx, fixations, avg_value_idx) %>%
#     tally() %>%
#     mutate(fixation_duration = n * 100, .keep = "unused")
#
#   data_set %>%
#     mutate(
#       trial_idx = seq_len(nrow(data_set)),
#       item_value = gsub("\\[|\\]", "", ss),
#     ) %>%
#     separate_rows(item_value, sep = ",") %>%
#     mutate(
#       item_value = gsub(" ", "", item_value, fixed = TRUE),
#       item_value = as.numeric(item_value)
#     ) %>%
#     group_by(trial_idx) %>%
#     mutate(value_idx = seq_len(n())) %>%
#     select(trial_idx, item_value, value_idx) %>%
#     left_join(first_fixation_duration, by = "trial_idx") %>%
#     group_by(trial_idx) %>%
#     filter(fixations == value_idx) %>%
#     mutate(item_value = round(item_value)) %>% # to create bins for plotting
#     ggplot(aes(item_value, fixation_duration)) +
#     stat_summary(fun.data = mean_se, geom = "errorbar") +
#     stat_summary(fun.data = mean_se, geom = "point", color = plt_colors[[plt_idx]]) +
#     stat_summary(fun = mean, geom = "line", size = .7, color = plt_colors[[plt_idx]]) +
#     labs(
#       x = "First fixated item rating",
#       y = "First fixation duration [ms]",
#       title = "First fixation duration as a function of the rating of the first-fixated item",
#       subtitle = paste0("Subset size: ", plt_idx)
#     ) +
#     theme_classic() +
#     theme(legend.position = "none") +
#     scale_x_continuous(breaks = c(-2, 0, 2)) -> plot5[[plt_idx]]
# }
#
# create_subplot <- function(plots, filename = NULL) {
#   # create subplots plot for each subset size
#   print((plots[[1]] + plots[[2]] + plots[[3]]) / (plots[[4]] + plots[[5]]))
#   if (!is.null(filename)) {
#     ggsave(here("figures", filename), dpi = 300)
#   }
# }

# create_subplot(plot1, "density_total_fixation_time.png")
# create_subplot(plot2, "histogram_number_of_fixations.png")
# create_subplot(plot3, "fixation_time_mean_value.png")
# create_subplot(plot4, "fixation_time_relative_value.png")
# create_subplot(plot5, "first_fixation_first_value.png")