# make_recovery_targets.R: summary statistics of each policy's simulated data (repetition 1, even-numbered trials
# with gaze), in the format of results/data_summary_by_k.csv, as fitting targets for the recovery analysis.
suppressMessages({library(dplyr); library(readr); library(here)})
MS <- 100
vs <- read_csv(here("results/value_sets.csv"), show_col_types = FALSE) %>% filter(has_gaze, trial %% 2 == 0)
rank_of <- function(ch, u0, u1, u2, u3) { u <- cbind(u0, u1, u2, u3); r <- t(apply(-u, 1, rank, ties.method = "first")); r[cbind(seq_len(nrow(u)), ch + 1)] }
for (pol in c("simultaneous", "sequential", "commit")) {
  tr <- read_csv(here(sprintf("results/model_%s_trials.csv", pol)), show_col_types = FALSE) %>% filter(rep == 1) %>%
    inner_join(vs %>% select(subject_id, trial, u_0:u_3), by = c("subject_id", "trial")) %>%
    mutate(r1 = rank_of(choice_1, u_0, u_1, u_2, u_3),
           r2 = ifelse(k >= 2, rank_of(ifelse(is.na(choice_2), 0, choice_2), u_0, u_1, u_2, u_3), NA),
           r3 = ifelse(k >= 3, rank_of(ifelse(is.na(choice_3), 0, choice_3), u_0, u_1, u_2, u_3), NA),
           in_order = case_when(k == 1 ~ NA, k == 2 ~ r1 < r2, TRUE ~ r1 < r2 & r2 < r3))
  out <- tr %>% group_by(k) %>% summarise(n_trials = n(), accuracy = mean(correct), mean_n_fix = mean(n_fix),
    mean_total_fix_time = mean(total_samples) * MS, p_first_resp_is_best = mean(first_is_best),
    p_in_rank_order = mean(in_order, na.rm = TRUE), mean_fix_time_before_resp1 = mean(resp_1) * MS, .groups = "drop")
  write_csv(out, here(sprintf("results/recovery/targets_%s.csv", pol)))
  cat(pol, ":\n"); print(as.data.frame(out), digits = 3)
}
