# tests.R: mixed-effects tests behind the Results. Writes results/tables/tests.txt.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(lme4); library(here)})
options(width = 120)
tr <- read_csv(here("results/data_trials.csv"), show_col_types = FALSE) %>% filter(rt_ok, has_gaze) %>%
  mutate(gap_z = boundary_gap / rating_sd, mean_z = (u_0 + u_1 + u_2 + u_3) / 4, kf = factor(k))
fx <- read_csv(here("results/data_fixations.csv"), show_col_types = FALSE) %>% filter(rt_ok) %>% semi_join(tr, by = c("subject_id", "trial")) %>% mutate(kf = factor(k))
sink(here("results/tables/tests.txt"))
cat("Mixed-effects tests (lme4). Random intercepts for participants; z = Wald statistic.\n\n")
pr <- function(m, title) { cat("==", title, "==\n"); print(round(coef(summary(m)), 4)); cat("\n") }

# 1. accuracy by boundary gap and k
pr(glmer(correct ~ gap_z * kf + (1 | subject_id), data = tr, family = binomial, nAGQ = 0), "P(chose top-k set) ~ gap x k")
# 2. total fixation time by gap, mean value, k
pr(lmer(log(total_fix_time) ~ (gap_z + mean_z) * kf + (1 | subject_id), data = tr), "log total fixation time ~ (gap + mean value) x k")
# 3. number of fixations
pr(lmer(n_fix ~ (gap_z + mean_z) * kf + (1 | subject_id), data = tr), "number of fixations ~ (gap + mean value) x k")
# 4. dwell share by rank x k (item level), with k=2 as reference so the k=1 and k=3 slopes are tested against it
sh <- tr %>% select(subject_id, trial, k, share_r1:share_r4) %>% pivot_longer(share_r1:share_r4, names_to = "rank", values_to = "share", names_prefix = "share_r") %>%
  mutate(rank_c = as.integer(rank) - 2.5, kf = relevel(factor(k), ref = "2"))
pr(lmer(share ~ rank_c * kf + (1 | subject_id), data = sh), "dwell share ~ rank x k (ref k = 2; rank centered)")
for (kk in 1:3) pr(lmer(share ~ rank_c + (1 | subject_id), data = filter(sh, k == kk)), paste0("dwell share ~ rank, k = ", kk))
# 5. phase-resolved dwell share by rank (k = 3): slope of rank by phase
ph <- fx %>% filter(k == 3, phase <= 3) %>% group_by(subject_id, trial, phase) %>%
  summarise(tot = sum(duration), r1 = sum(duration[rank == 1]), r2 = sum(duration[rank == 2]), r3 = sum(duration[rank == 3]), r4 = sum(duration[rank == 4]), .groups = "drop") %>%
  pivot_longer(r1:r4, names_to = "rank", values_to = "d", names_prefix = "r") %>% mutate(share = d / tot, rank_c = as.integer(rank) - 2.5, phase = factor(phase))
pr(lmer(share ~ rank_c * phase + (1 | subject_id), data = ph), "k = 3: dwell share within phase ~ rank x phase (ref phase 1)")
for (p in 1:3) pr(lmer(share ~ rank_c + (1 | subject_id), data = filter(ph, phase == p)), paste0("k = 3, phase ", p, ": share ~ rank"))
# 6. before the first response: does the rank slope differ by k?
ph1 <- fx %>% filter(phase == 1) %>% group_by(subject_id, trial, k) %>%
  summarise(tot = sum(duration), r1 = sum(duration[rank == 1]), r2 = sum(duration[rank == 2]), r3 = sum(duration[rank == 3]), r4 = sum(duration[rank == 4]), .groups = "drop") %>%
  pivot_longer(r1:r4, names_to = "rank", values_to = "d", names_prefix = "r") %>% mutate(share = d / tot, rank_c = as.integer(rank) - 2.5, kf = relevel(factor(k), ref = "1"))
pr(lmer(share ~ rank_c * kf + (1 | subject_id), data = ph1), "before the first response: share ~ rank x k (ref k = 1)")
# 7. first fixation duration by rank and k
ff <- fx %>% filter(is_first, n_fix >= 2) %>% mutate(rank_c = rank - 2.5)
pr(lmer(log(duration) ~ rank_c * kf + (1 | subject_id), data = ff), "log first fixation duration ~ rank x k")
# 8. gaze entropy
pr(lmer(gaze_entropy ~ (gap_z + mean_z) * kf + (1 | subject_id), data = tr), "gaze entropy ~ (gap + mean value) x k")
# 9. response order (k >= 2): first response is best; in rank order, vs chance
ro <- tr %>% filter(k >= 2)
cat("== response order ==\n")
print(ro %>% group_by(k) %>% summarise(p_first_best = mean(first_is_best), p_in_order = mean(in_rank_order, na.rm = TRUE), chance_order = 1 / factorial(first(k))))
# chance that the first response is the best given the chosen set is top-k: 1/k; test against 1/k
for (kk in 2:3) { m <- glmer(first_is_best ~ 1 + (1 | subject_id), data = filter(ro, k == kk), family = binomial, nAGQ = 0, offset = rep(qlogis(1 / kk), sum(ro$k == kk))); cat(sprintf("k = %d: first response is best vs chance 1/k: intercept %.3f (z = %.2f)\n", kk, fixef(m), coef(summary(m))[1, 3])) }
# 10. last fixation to a chosen item, and choice by dwell share (item level)
it <- fx %>% group_by(subject_id, trial, k, item, rank) %>% summarise(d = sum(duration), chosen = any(is_chosen), .groups = "drop") %>%
  group_by(subject_id, trial) %>% mutate(share = d / sum(d)) %>% ungroup() %>% mutate(kf = factor(k), rank_c = rank - 2.5)
pr(glmer(chosen ~ share + rank_c + kf + (1 | subject_id), data = it, family = binomial, nAGQ = 0), "P(item chosen) ~ dwell share + rank + k")
gq <- read_csv(here("results/tables/gaze_quality_by_subject.csv"), show_col_types = FALSE) %>% filter(subject_id %in% tr$subject_id) %>% mutate(better = on_roi >= median(on_roi))
cat(sprintf("== tracking quality: %d participants in the gaze analyses; %.2f million raw samples in choice trials, %.1f%% on an item; per-participant median %.1f%%, range %.1f-%.1f%%; split at the median ==\n",
            nrow(gq), sum(gq$n_samples) / 1e6, 100 * weighted.mean(gq$on_roi, gq$n_samples), 100 * median(gq$on_roi), 100 * min(gq$on_roi), 100 * max(gq$on_roi)))
for (b in c(TRUE, FALSE)) {
  ids <- gq$subject_id[gq$better == b]; lab <- if (b) "better-tracked half" else "worse-tracked half"
  pr(lmer(share ~ rank_c + (1 | subject_id), data = filter(sh, k == 1, subject_id %in% ids)), paste0("dwell share ~ rank, k = 1, ", lab))
  for (p in 1:3) pr(lmer(share ~ rank_c + (1 | subject_id), data = filter(ph, phase == p, subject_id %in% ids)), paste0("k = 3, phase ", p, ": share ~ rank, ", lab))
}
sink()
cat(readLines(here("results/tables/tests.txt")), sep = "\n")
