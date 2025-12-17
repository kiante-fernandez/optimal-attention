
library(here)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
library(stringr)
glue = glue::glue

theme_set(theme_bw(base_size = 12))
theme_update(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
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

update_geom_defaults("line", list(linewidth = 1.2))

date_stamp = format(Sys.Date(), "%Y-%m-%d")

# %% ==================== Load Model Data ====================

model_fixations = read_csv(here::here("results/fixations.csv"), show_col_types=F) %>%
    mutate(k = factor(sub_size), source = "Model")

# %% ==================== Load and Process Empirical Data ====================

raw_data = read_csv(here::here("data/choosekEYE_R.csv"), show_col_types=F)

# Map button_ROI to item index (i=0, j=1, k=2, l=3)
roi_to_idx = c("i" = 0, "j" = 1, "k" = 2, "l" = 3)

# Define correct trials: selecting the k highest-valued items
# For k=1: choice_rank_1 == 1
# For k=2: choice_rank_1 + choice_rank_2 == 3 (i.e., ranks 1 and 2)
# For k=3: choice_rank_1 + choice_rank_2 + choice_rank_3 == 6 (i.e., ranks 1, 2, and 3)
correct_trials = raw_data %>%
    select(subject_id, trial, options_selected, choice_rank_1, choice_rank_2, choice_rank_3) %>%
    distinct() %>%
    mutate(
        is_correct = case_when(
            options_selected == 1 ~ choice_rank_1 == 1,
            options_selected == 2 ~ (choice_rank_1 + choice_rank_2) == 3,
            options_selected == 3 ~ (choice_rank_1 + choice_rank_2 + choice_rank_3) == 6,
            TRUE ~ FALSE
        )
    ) %>%
    filter(is_correct) %>%
    select(subject_id, trial)

# Process empirical data to match model format (correct trials only)
empirical_fixations = raw_data %>%
    inner_join(correct_trials, by = c("subject_id", "trial")) %>%  # Keep only correct trials
    filter(button_ROI %in% c("i", "j", "k", "l")) %>%
    filter(options_selected %in% c(1, 2, 3)) %>%  # k can only be 1,2,3 with 4 items
    mutate(
        fixated_idx = roi_to_idx[button_ROI],
        # Get the value of the fixated item
        fixated_val_raw = case_when(
            fixated_idx == 0 ~ item_value_0,
            fixated_idx == 1 ~ item_value_1,
            fixated_idx == 2 ~ item_value_2,
            fixated_idx == 3 ~ item_value_3
        ),
            k = factor(options_selected)
    ) %>%
    # Aggregate by subject, trial, and fixation to get duration
    group_by(subject_id, trial, k, fixation_id, fixation_category, fixated_idx, fixated_val_raw) %>%
    summarise(
        duration = n(),  # number of samples = duration
        .groups = "drop"
    ) %>%
    # Z-score normalize fixated values within each subject (to match model's N(0,1) value distribution)
    group_by(subject_id) %>%
    mutate(
        fixated_val = (fixated_val_raw - mean(fixated_val_raw)) / sd(fixated_val_raw)
    ) %>%
    ungroup() %>%
    rename(fix_num = fixation_id) %>%
    mutate(source = "Data")

# %% ==================== First Fixation Duration Plot ====================

# Model first fixations
model_first = model_fixations %>%
    filter(fix_num == 1) %>%
    select(k, fixated_val, duration, source)

# Empirical first fixations
data_first = empirical_fixations %>%
    filter(fix_num == 1) %>%
    select(k, fixated_val, duration, source)

# Combine and set factor levels so Model=solid, Data=dashed (alphabetical would reverse this)
combined_first = bind_rows(model_first, data_first) %>%
    mutate(source = factor(source, levels = c("Model", "Data")))

# Individual subject lines for data
data_first_by_subject = empirical_fixations %>%
    filter(fix_num == 1) %>%
    group_by(k, subject_id) %>%
    mutate(val_bin = cut(fixated_val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, subject_id, val_bin) %>%
    summarise(duration = mean(duration), fixated_val = mean(fixated_val), .groups="drop")

# Plot
p1 = combined_first %>%
    ggplot(aes(fixated_val, duration, color = k)) +
    # Individual subject lines (data only)
    stat_summary_bin(data = data_first_by_subject,
                     aes(group = interaction(k, subject_id)),
                     fun=mean, geom="line", breaks=seq(-3, 3, 1),
                     alpha=0.15, linewidth=0.5) +
    # Aggregate lines
    stat_summary_bin(aes(linetype = source), fun=mean, geom="line",
                     breaks=seq(-3, 3, 1), linewidth=1.2) +
    facet_wrap(~k) +
    k_colors +
    labs(
        x = "Fixated Item Value (normalized)",
        y = "First Fixation Duration",
        title = "First Fixation Duration by Item Value",
        linetype = "Source"
    )

ggsave(here::here(glue("figures/model_data_first_duration_{date_stamp}.png")), p1, height=5, width=8)

# %% ==================== Total Proportion Plot ====================

# Model proportions
model_prop = model_fixations %>%
    group_by(k, trial_idx, fixated) %>%
    summarise(total_duration=sum(duration), val=mean(fixated_val), .groups="drop") %>%
    group_by(k, trial_idx) %>%
    mutate(prop_duration=total_duration / sum(total_duration)) %>%
    ungroup() %>%
    mutate(source = "Model")

# Empirical proportions
data_prop = empirical_fixations %>%
    group_by(k, subject_id, trial, fixated_idx) %>%
    summarise(total_duration=sum(duration), val=mean(fixated_val), .groups="drop") %>%
    group_by(k, subject_id, trial) %>%
    mutate(prop_duration=total_duration / sum(total_duration)) %>%
    ungroup() %>%
    mutate(source = "Data")

# Combine and set factor levels so Model=solid, Data=dashed
combined_prop = bind_rows(
    model_prop %>% select(k, val, prop_duration, source),
    data_prop %>% select(k, val, prop_duration, source)
) %>%
    mutate(source = factor(source, levels = c("Model", "Data")))

# Individual subject proportions for data
data_prop_by_subject = data_prop %>%
    group_by(k, subject_id) %>%
    mutate(val_bin = cut(val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, subject_id, val_bin) %>%
    summarise(prop_duration = mean(prop_duration), val = mean(val), .groups="drop")

# Plot
p2 = combined_prop %>%
    ggplot(aes(val, prop_duration, color = k)) +
    # Individual subject lines (data only)
    stat_summary_bin(data = data_prop_by_subject,
                     aes(group = interaction(k, subject_id)),
                     fun=mean, geom="line", breaks=seq(-3, 3, 1),
                     alpha=0.15, linewidth=0.5) +
    # Aggregate lines
    stat_summary_bin(aes(linetype = source), fun=mean, geom="line",
                     breaks=seq(-3, 3, 1), linewidth=1.2) +
    facet_wrap(~k) +
    k_colors +
    labs(
        x = "Item Value (normalized)",
        y = "Proportion of Total Fixation Time",
        title = "Attention Allocation by Item Value",
        linetype = "Source"
    )

ggsave(here::here(glue("figures/model_data_proportion_{date_stamp}.png")), p2, height=5, width=8)

# %% ==================== Summary Stats ====================

cat("\n=== Model Summary ===\n")
model_fixations %>%
    group_by(k) %>%
    summarise(
        n_trials = n_distinct(trial_idx),
        mean_duration = mean(duration),
        n_fixations = n()
    ) %>%
    print()

cat("\n=== Data Summary ===\n")
empirical_fixations %>%
    group_by(k) %>%
    summarise(
        n_trials = n_distinct(paste(subject_id, trial)),
        mean_duration = mean(duration),
        n_fixations = n()
    ) %>%
    print()

cat(glue("\n\nFigures saved to figures/ with date stamp {date_stamp}\n"))
