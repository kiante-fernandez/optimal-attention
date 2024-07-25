
library(here)
library(ggplot2)
library(readr)
library(tidyr)
library(magrittr)
library(stringr)
library(tidyverse)
glue = glue::glue

theme_set(theme_bw(base_size = 12))
theme_update(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_line(color="#EDEDED"),
    strip.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(size=12),
    legend.position="right",
    panel.spacing = unit(1, "lines"),
)

k_colors = scale_colour_manual(
    values=c(
        "#d7191c",
        "#fdae61",
        "#ADADAD",
        "#abd9e9",
        "#2c7bb6"
    ))

update_geom_defaults("line", list(size = 1.2))

# %% --------

load_sim_data <- function(name, col_types=cols()) {
    path = here::here("simulation_results")
    read_csv(glue("{path}/{name}.csv"), show_col_types=F, col_types=col_types) %>%
        mutate(k = factor(sub_size), .keep="unused")
}

trials = load_sim_data("trials", cols(attended=col_character()))
fixations = load_sim_data("fixations")

# %% --------

fixations %>% 
    filter(fix_num == 1) %>% 
    ggplot(aes(fixated_val, duration, color = k)) +
    stat_summary_bin(fun=mean, geom="line", breaks=-3:3) +
    facet_wrap(~k) + 
    k_colors

ggsave("figures/first_duration.png", height=5)

# %% --------

fixations %>% 
    group_by(k, trial_idx, fixated) %>% 
    summarise(total_duration=sum(duration), val=mean(fixated_val)) %>% 
    mutate(prop_duration=total_duration / sum(total_duration)) %>% 
    ggplot(aes(val, prop_duration, color = k)) +
    stat_summary_bin(fun=mean, geom="line", breaks=-3:3) +
    facet_wrap(~k) +
    k_colors

ggsave("figures/total_proportion.png", height=5)
