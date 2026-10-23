library(here)
library(readr)
library(dplyr)
library(tidyr)

# Define exclude_rt function (from DRAFT_exp_1_choosek_regresssions.R)
exclude_rt <- function(df, rt_column = "total_rt", lower_bound = 250, upper_bound = 10000, iqr_multiplier = 2) {
  df %>%
    filter(!!sym(rt_column) > lower_bound) %>%
    filter(!!sym(rt_column) < upper_bound) %>%
    group_by(subject_id) %>%
    mutate(
      Q1 = quantile(!!sym(rt_column), .25),
      Q3 = quantile(!!sym(rt_column), .75),
      IQR = IQR(!!sym(rt_column))
    ) %>%
    filter(!!sym(rt_column) > (Q1 - iqr_multiplier * IQR) & !!sym(rt_column) < (Q3 + iqr_multiplier * IQR)) %>%
    ungroup()
}

# Load data
model_fixations = read_csv(here::here("results/fixations.csv"), show_col_types=F) %>%
    mutate(k = factor(sub_size), source = "Model")

raw_data = read_csv(here::here("data/choosekEYE_R.csv"), show_col_types=F)

roi_to_idx = c("j" = 0, "i" = 1, "l" = 2, "k" = 3)  # positional mapping, see model_data_comparison.R

# Get trial-level data with RT (following the correct method)
trial_data = raw_data %>%
    select(subject_id, trial, options_selected, rt_1, rt_2, rt_3,
           choice_rank_1, choice_rank_2, choice_rank_3) %>%
    distinct() %>%
    # Convert to inter-response times and calculate total_rt
    mutate(
        rt_3_irt = rt_3 - rt_2,
        rt_2_irt = rt_2 - rt_1
    ) %>%
    mutate(
        total_rt = rt_1 + ifelse(is.na(rt_2_irt), 0, rt_2_irt) + ifelse(is.na(rt_3_irt), 0, rt_3_irt)
    )

cat("=== RT Distribution Before Exclusion ===\n")
cat(sprintf("N trials: %d\n", nrow(trial_data)))
cat(sprintf("RT range: %.0f - %.0f ms\n", min(trial_data$total_rt, na.rm=T), max(trial_data$total_rt, na.rm=T)))

# Apply RT exclusion
trial_data_filtered = trial_data %>%
    exclude_rt()

cat(sprintf("\n=== After RT Exclusion ===\n"))
cat(sprintf("N trials: %d (%.1f%% retained)\n",
            nrow(trial_data_filtered),
            100 * nrow(trial_data_filtered) / nrow(trial_data)))

# Define correct trials
trial_data_filtered = trial_data_filtered %>%
    mutate(
        is_correct = case_when(
            options_selected == 1 ~ choice_rank_1 == 1,
            options_selected == 2 ~ (choice_rank_1 + choice_rank_2) == 3,
            options_selected == 3 ~ (choice_rank_1 + choice_rank_2 + choice_rank_3) == 6,
            TRUE ~ FALSE
        )
    )

valid_correct_trials = trial_data_filtered %>%
    filter(is_correct) %>%
    select(subject_id, trial)

cat(sprintf("N correct trials after RT exclusion: %d\n", nrow(valid_correct_trials)))

# Process empirical fixations with RT exclusion applied
empirical_fixations = raw_data %>%
    inner_join(valid_correct_trials, by = c("subject_id", "trial")) %>%
    filter(button_ROI %in% c("i", "j", "k", "l")) %>%
    filter(options_selected %in% c(1, 2, 3)) %>%
    mutate(
        fixated_idx = roi_to_idx[button_ROI],
        fixated_val_raw = case_when(
            fixated_idx == 0 ~ item_value_0,
            fixated_idx == 1 ~ item_value_1,
            fixated_idx == 2 ~ item_value_2,
            fixated_idx == 3 ~ item_value_3
        ),
        k = factor(options_selected)
    ) %>%
    group_by(subject_id, trial, k, fixation_id, fixation_category, fixated_idx, fixated_val_raw) %>%
    summarise(duration = n(), .groups = "drop") %>%
    group_by(subject_id) %>%
    mutate(fixated_val = (fixated_val_raw - mean(fixated_val_raw)) / sd(fixated_val_raw)) %>%
    ungroup() %>%
    rename(fix_num = fixation_id) %>%
    mutate(source = "Data")

cat("\n=== QUANTITATIVE FIT METRICS (WITH CORRECT RT EXCLUSION) ===\n\n")

# 1. First Fixation Duration Analysis
cat("--- First Fixation Duration by Value ---\n\n")

model_first = model_fixations %>%
    filter(fix_num == 1) %>%
    mutate(val_bin = cut(fixated_val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, val_bin) %>%
    summarise(mean_dur = mean(duration), .groups="drop")

data_first = empirical_fixations %>%
    filter(fix_num == 1) %>%
    mutate(val_bin = cut(fixated_val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, val_bin) %>%
    summarise(mean_dur = mean(duration), .groups="drop")

first_merged = inner_join(
    model_first %>% rename(model_dur = mean_dur),
    data_first %>% rename(data_dur = mean_dur),
    by = c("k", "val_bin")
)

for (kval in c("1", "2", "3")) {
    subset = first_merged %>% filter(k == kval)
    if (nrow(subset) > 2) {
        r = cor(subset$model_dur, subset$data_dur, use="complete.obs")
        cat(sprintf("k=%s: Correlation r = %.3f (n=%d bins)\n", kval, r, nrow(subset)))
    }
}

overall_r = cor(first_merged$model_dur, first_merged$data_dur, use="complete.obs")
cat(sprintf("\nOverall correlation (first fixation): r = %.3f\n\n", overall_r))

# 2. Proportion of Fixation Time Analysis
cat("--- Proportion of Fixation Time by Value ---\n\n")

model_prop = model_fixations %>%
    group_by(k, trial_idx, fixated) %>%
    summarise(total_duration=sum(duration), val=mean(fixated_val), .groups="drop") %>%
    group_by(k, trial_idx) %>%
    mutate(prop_duration=total_duration / sum(total_duration)) %>%
    ungroup() %>%
    mutate(val_bin = cut(val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, val_bin) %>%
    summarise(mean_prop = mean(prop_duration), .groups="drop")

data_prop = empirical_fixations %>%
    group_by(k, subject_id, trial, fixated_idx) %>%
    summarise(total_duration=sum(duration), val=mean(fixated_val), .groups="drop") %>%
    group_by(k, subject_id, trial) %>%
    mutate(prop_duration=total_duration / sum(total_duration)) %>%
    ungroup() %>%
    mutate(val_bin = cut(val, breaks=seq(-3, 3, 1), include.lowest=TRUE)) %>%
    group_by(k, val_bin) %>%
    summarise(mean_prop = mean(prop_duration), .groups="drop")

prop_merged = inner_join(
    model_prop %>% rename(model_prop = mean_prop),
    data_prop %>% rename(data_prop = mean_prop),
    by = c("k", "val_bin")
)

for (kval in c("1", "2", "3")) {
    subset = prop_merged %>% filter(k == kval)
    if (nrow(subset) > 2) {
        r = cor(subset$model_prop, subset$data_prop, use="complete.obs")
        cat(sprintf("k=%s: Correlation r = %.3f (n=%d bins)\n", kval, r, nrow(subset)))
    }
}

overall_prop_r = cor(prop_merged$model_prop, prop_merged$data_prop, use="complete.obs")
cat(sprintf("\nOverall correlation (proportion): r = %.3f\n\n", overall_prop_r))

# 3. Slope Analysis
cat("--- Slope of Value-Duration Relationship ---\n\n")

model_slopes = c()
data_slopes = c()
for (kval in c("1", "2", "3")) {
    model_subset = model_fixations %>% filter(fix_num == 1, k == kval)
    data_subset = empirical_fixations %>% filter(fix_num == 1, k == kval)

    ms = coef(lm(duration ~ fixated_val, data=model_subset))[2]
    ds = coef(lm(duration ~ fixated_val, data=data_subset))[2]
    model_slopes = c(model_slopes, ms)
    data_slopes = c(data_slopes, ds)

    cat(sprintf("k=%s: Model slope = %.3f, Data slope = %.3f\n", kval, ms, ds))
}

# 4. Pattern Agreement
cat("\n--- Pattern Agreement ---\n")
for (i in 1:3) {
    kval = c("1", "2", "3")[i]
    direction = ifelse(sign(model_slopes[i]) == sign(data_slopes[i]), "AGREE", "DISAGREE")
    cat(sprintf("  k=%s: %s (model: %s, data: %s)\n",
                kval, direction,
                ifelse(model_slopes[i] > 0, "positive", "negative"),
                ifelse(data_slopes[i] > 0, "positive", "negative")))
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Overall first fixation correlation: r = %.3f\n", overall_r))
cat(sprintf("Overall proportion correlation: r = %.3f\n", overall_prop_r))
cat(sprintf("Pattern agreement: %d/3 k-conditions\n", sum(sign(model_slopes) == sign(data_slopes))))
