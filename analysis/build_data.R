# build_data.R: build the trial-, fixation-, and value-set tables used by every analysis.
# Run from the repository root: Rscript analysis/build_data.R
#
# Inputs:  data/choosekEYE_R.csv.gz  (gaze samples on item ROIs; t in ms from trial onset)
#          data/choosek_R.csv     (trial-level behavior, all trials)
# Outputs: results/data_trials.csv, results/data_fixations.csv, results/value_sets.csv,
#          results/data_summary_by_k.csv, results/data_subject_ratings.csv
#
# Conventions. Items are indexed by screen position 0..3 = left, top, right, bottom = keys
# j, i, l, k (verified against key_press vs choice_1). The dwell_0..3 columns in the gaze file
# use a different order (i, j, l, k) and are not used. A fixation is a contiguous span of gaze
# on one item; consecutive fixation_ids on the same item are merged. Durations are in ms:
# last sample minus first sample plus one median inter-sample interval. Ranks: 1 = best.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(purrr); library(here)})

EXCLUDE_SUBJ <- "P05"   # condition-assignment bug (see choose_k)
roi_to_idx   <- c(j = 0, i = 1, l = 2, k = 3)
RT_LOW <- 250; RT_HIGH <- 10000; IQR_MULT <- 2   # pre-registered RT exclusion

# ---------------------------------------------------------------- behavior (all trials)
beh <- read_csv(here("data/choosek_R.csv"), show_col_types = FALSE) %>%
  filter(condition %in% 1:3, subject_id != EXCLUDE_SUBJ) %>%
  rename(k = condition) %>%
  mutate(total_rt = case_when(k == 1 ~ rt_1, k == 2 ~ rt_2, TRUE ~ rt_3),
         irt_1 = rt_1, irt_2 = rt_2 - rt_1, irt_3 = rt_3 - rt_2)

# per-subject rating distribution over the 60 items (for standardizing values)
ratings <- beh %>%
  select(subject_id, starts_with("item_name_"), starts_with("item_value_")) %>%
  pivot_longer(-subject_id, names_to = c(".value", "pos"), names_pattern = "item_(name|value)_(\\d)") %>%
  distinct(subject_id, name, value) %>%
  group_by(subject_id) %>%
  summarise(n_items = n(), rating_mean = mean(value), rating_sd = sd(value), .groups = "drop")
write_csv(ratings, here("results/data_subject_ratings.csv"))

trials <- beh %>%
  left_join(ratings, by = "subject_id") %>%
  mutate(
    u_0 = (item_value_0 - rating_mean) / rating_sd, u_1 = (item_value_1 - rating_mean) / rating_sd,
    u_2 = (item_value_2 - rating_mean) / rating_sd, u_3 = (item_value_3 - rating_mean) / rating_sd,
    value_best   = pmax(item_value_0, item_value_1, item_value_2, item_value_3),
    value_mean   = (item_value_0 + item_value_1 + item_value_2 + item_value_3) / 4,
    # k-th and (k+1)-th best values define the selection boundary
    boundary_gap = pmap_dbl(list(item_value_0, item_value_1, item_value_2, item_value_3, k),
                            function(a, b, c, d, kk) { v <- sort(c(a, b, c, d), decreasing = TRUE); v[kk] - v[kk + 1] }),
    n_chosen_in_topk = (choice_rank_1 <= k) + ifelse(k >= 2, choice_rank_2 <= k, 0) + ifelse(k >= 3, choice_rank_3 <= k, 0),
    correct = n_chosen_in_topk == k,                      # chose exactly the top-k set
    first_is_best = choice_rank_1 == 1,
    in_rank_order = case_when(k == 1 ~ NA, k == 2 ~ choice_rank_1 < choice_rank_2,
                              TRUE ~ choice_rank_1 < choice_rank_2 & choice_rank_2 < choice_rank_3)
  ) %>%
  group_by(subject_id) %>%
  mutate(rt_in_range = total_rt > RT_LOW & total_rt < RT_HIGH,
         q1 = quantile(total_rt[rt_in_range], .25), q3 = quantile(total_rt[rt_in_range], .75),
         rt_ok = rt_in_range & total_rt > q1 - IQR_MULT * (q3 - q1) & total_rt < q3 + IQR_MULT * (q3 - q1)) %>%
  ungroup() %>% select(-q1, -q3, -rt_in_range)

# ---------------------------------------------------------------- gaze
raw <- read_csv(here("data/choosekEYE_R.csv.gz"), show_col_types = FALSE,
                col_select = c(t, subject_id, trial, options_selected, button_ROI, fixation_id)) %>%
  filter(options_selected %in% 1:3, subject_id != EXCLUDE_SUBJ, button_ROI %in% names(roi_to_idx)) %>%
  mutate(item = unname(roi_to_idx[button_ROI]))

median_dt <- raw %>% arrange(subject_id, trial, t) %>% group_by(subject_id, trial) %>%
  mutate(dt = t - lag(t)) %>% ungroup() %>% filter(dt > 0) %>% pull(dt) %>% median()
cat(sprintf("median inter-sample interval: %d ms\n", median_dt))

fix_raw <- raw %>%
  group_by(subject_id, trial, fixation_id) %>%
  summarise(item = { tb <- table(item); as.integer(names(tb)[which.max(tb)]) },   # modal item within the fixation id
            onset = min(t), last = max(t), n_samples = n(), .groups = "drop") %>%
  arrange(subject_id, trial, onset)

# merge consecutive fixation ids on the same item into one fixation
fixations <- fix_raw %>%
  group_by(subject_id, trial) %>%
  mutate(run = cumsum(item != lag(item, default = -1L))) %>%
  group_by(subject_id, trial, run) %>%
  summarise(item = first(item), onset = min(onset), last = max(last), n_samples = sum(n_samples),
            n_ids_merged = n(), .groups = "drop") %>%
  mutate(offset = last + median_dt, duration = offset - onset) %>%
  select(-run, -last)
cat(sprintf("fixation ids: %d; after merging same-item runs: %d\n", nrow(fix_raw), nrow(fixations)))

fixations <- fixations %>%
  inner_join(trials %>% select(subject_id, trial, k, rt_ok, total_rt, rt_1, rt_2, rt_3,
                               item_value_0:item_value_3, item_rank_0:item_rank_3, u_0:u_3,
                               choice_1, choice_2, choice_3, correct),
             by = c("subject_id", "trial")) %>%
  group_by(subject_id, trial) %>%
  arrange(onset, .by_group = TRUE) %>%
  mutate(
    fix_num = row_number(), n_fix = n(), is_first = fix_num == 1, is_last = fix_num == n_fix,
    value_raw = case_when(item == 0 ~ item_value_0, item == 1 ~ item_value_1, item == 2 ~ item_value_2, TRUE ~ item_value_3),
    value_z   = case_when(item == 0 ~ u_0, item == 1 ~ u_1, item == 2 ~ u_2, TRUE ~ u_3),
    rank      = case_when(item == 0 ~ item_rank_0, item == 1 ~ item_rank_1, item == 2 ~ item_rank_2, TRUE ~ item_rank_3),
    chosen_at = case_when(item == choice_1 ~ 1L, !is.na(choice_2) & item == choice_2 ~ 2L,
                          !is.na(choice_3) & item == choice_3 ~ 3L, TRUE ~ NA_integer_),
    is_chosen = !is.na(chosen_at),
    # response phase: which keypress this fixation precedes (k + 1 = after the final keypress)
    phase = case_when(onset < rt_1 ~ 1L, k >= 2 & onset < rt_2 ~ 2L, k == 3 & onset < rt_3 ~ 3L, TRUE ~ k + 1L),
    prev_item = lag(item), is_refixation = map2_lgl(fix_num, item, ~ .y %in% item[seq_len(.x - 1)]),
    cum_self = { d <- c(0, cumsum(duration))[fix_num]; sapply(seq_along(item), function(i) sum(duration[seq_len(i - 1)][item[seq_len(i - 1)] == item[i]])) },
    cum_total_before = c(0, cumsum(duration))[fix_num],
    fix_advantage = cum_self - (cum_total_before - cum_self) / 3,
    time_from_response = case_when(phase == 1 ~ onset - rt_1, phase == 2 ~ onset - rt_2, phase == 3 ~ onset - rt_3, TRUE ~ NA_real_)
  ) %>%
  ungroup() %>%
  select(subject_id, trial, k, rt_ok, fix_num, n_fix, is_first, is_last, item, onset, offset, duration, n_samples,
         n_ids_merged, value_raw, value_z, rank, chosen_at, is_chosen, phase, prev_item, is_refixation,
         cum_self, cum_total_before, fix_advantage, time_from_response, total_rt, rt_1, rt_2, rt_3, correct)
write_csv(fixations, here("results/data_fixations.csv"))

# ---------------------------------------------------------------- trial-level gaze summaries
gaze_trial <- fixations %>%
  group_by(subject_id, trial) %>%
  summarise(n_fix = n(), total_fix_time = sum(duration),
            n_items_fixated = n_distinct(item),
            all4_in_first4 = n_fix >= 4 && n_distinct(item[1:4]) == 4,
            first_item = item[1], first_rank = rank[1], first_duration = duration[1],
            last_item = item[n()], last_rank = rank[n()], last_duration = duration[n()],
            last_is_chosen = is_chosen[n()],
            dwell_r1 = sum(duration[rank == 1]), dwell_r2 = sum(duration[rank == 2]),
            dwell_r3 = sum(duration[rank == 3]), dwell_r4 = sum(duration[rank == 4]),
            fix_time_before_resp1 = sum(duration[phase == 1]),
            dwell_item0 = sum(duration[item == 0]), dwell_item1 = sum(duration[item == 1]),
            dwell_item2 = sum(duration[item == 2]), dwell_item3 = sum(duration[item == 3]),
            .groups = "drop") %>%
  mutate(share_r1 = dwell_r1 / total_fix_time, share_r2 = dwell_r2 / total_fix_time,
         share_r3 = dwell_r3 / total_fix_time, share_r4 = dwell_r4 / total_fix_time,
         gaze_entropy = pmap_dbl(list(share_r1, share_r2, share_r3, share_r4),
                                 function(...) { p <- c(...); p <- p[p > 0]; -sum(p * log2(p)) }))

trials <- trials %>% left_join(gaze_trial, by = c("subject_id", "trial")) %>%
  mutate(has_gaze = !is.na(n_fix))
write_csv(trials, here("results/data_trials.csv"))

# value sets for the model (one row per trial with usable RT; all trials, with or without gaze)
trials %>% filter(rt_ok) %>%
  select(subject_id, trial, k, has_gaze, item_value_0:item_value_3, u_0:u_3, rating_mean, rating_sd,
         choice_1, choice_2, choice_3) %>%
  write_csv(here("results/value_sets.csv"))

# summary statistics by k used as fitting targets (trials with gaze and usable RT)
tg <- trials %>% filter(rt_ok, has_gaze)
summary_k <- tg %>% group_by(k) %>%
  summarise(n_trials = n(), n_subjects = n_distinct(subject_id),
            accuracy = mean(correct), mean_n_fix = mean(n_fix), median_n_fix = median(n_fix),
            mean_total_fix_time = mean(total_fix_time), median_total_fix_time = median(total_fix_time),
            mean_total_rt = mean(total_rt), median_total_rt = median(total_rt),
            share_r1 = mean(share_r1), share_r2 = mean(share_r2), share_r3 = mean(share_r3), share_r4 = mean(share_r4),
            mean_entropy = mean(gaze_entropy), p_all4_in_first4 = mean(all4_in_first4[n_fix >= 4]),
            p_single_fix = mean(n_fix == 1), p_last_is_chosen = mean(last_is_chosen),
            p_first_is_best = mean(first_rank == 1), p_first_resp_is_best = mean(first_is_best),
            p_in_rank_order = mean(in_rank_order, na.rm = TRUE), mean_fix_time_before_resp1 = mean(fix_time_before_resp1), .groups = "drop")
write_csv(summary_k, here("results/data_summary_by_k.csv"))

cat("\n=== trials ===\n"); print(trials %>% count(k, rt_ok, has_gaze))
cat("\n=== summary by k (rt_ok & has_gaze) ===\n"); print(as.data.frame(summary_k), digits = 3)
cat("\nfixation duration (ms) quantiles:\n"); print(quantile(fixations$duration, c(.05, .25, .5, .75, .95)))
cat("\nwritten: results/data_trials.csv, results/data_fixations.csv, results/value_sets.csv, results/data_summary_by_k.csv\n")
