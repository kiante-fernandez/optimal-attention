# descriptives.R: eye-movement and behavioral descriptives by selection size, for the Methods/Results.
# Run after build_data.R. Writes results/tables/descriptives_by_k.csv and prints a markdown table.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(here)})
trials <- read_csv(here("results/data_trials.csv"), show_col_types = FALSE)
fix    <- read_csv(here("results/data_fixations.csv"), show_col_types = FALSE)

tg <- trials %>% filter(rt_ok, has_gaze)
fg <- fix %>% filter(rt_ok)

# subject-level means first, then the mean (and SD) over subjects
subj_mean <- function(df, var, by = "k") {
  df %>% group_by(subject_id, .data[[by]]) %>% summarise(v = mean({{ var }}, na.rm = TRUE), .groups = "drop") %>%
    group_by(.data[[by]]) %>% summarise(mean = mean(v), sd = sd(v), .groups = "drop")
}
fmt <- function(m, s, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f)"), m, s)

rows <- list(
  trials %>% group_by(k) %>% summarise(stat = "Trials (all / usable RT / with gaze)",
                                       value = sprintf("%d / %d / %d", n(), sum(rt_ok), sum(rt_ok & has_gaze)), .groups = "drop"),
  subj_mean(tg, total_rt)         %>% transmute(k, stat = "Response time, last keypress (ms)", value = fmt(mean, sd, 0)),
  subj_mean(tg, correct)          %>% transmute(k, stat = "Chose exactly the top-k set",         value = fmt(mean, sd)),
  subj_mean(tg %>% filter(k >= 2), first_is_best) %>% transmute(k, stat = "First response is the best item", value = fmt(mean, sd)),
  subj_mean(tg %>% filter(k >= 2), in_rank_order) %>% transmute(k, stat = "Responses in rank order",       value = fmt(mean, sd)),
  subj_mean(tg, n_fix)            %>% transmute(k, stat = "Fixations per trial",                 value = fmt(mean, sd)),
  subj_mean(tg, total_fix_time)   %>% transmute(k, stat = "Total fixation time (ms)",            value = fmt(mean, sd, 0)),
  subj_mean(tg, total_fix_time / total_rt) %>% transmute(k, stat = "Fraction of trial time on items", value = fmt(mean, sd)),
  subj_mean(fg, duration)         %>% transmute(k, stat = "Fixation duration, all (ms)",         value = fmt(mean, sd, 0)),
  subj_mean(fg %>% filter(is_first), duration) %>% transmute(k, stat = "First fixation duration (ms)", value = fmt(mean, sd, 0)),
  subj_mean(fg %>% filter(!is_first, !is_last), duration) %>% transmute(k, stat = "Middle fixation duration (ms)", value = fmt(mean, sd, 0)),
  subj_mean(fg %>% filter(is_last), duration)  %>% transmute(k, stat = "Last fixation duration (ms)",  value = fmt(mean, sd, 0)),
  subj_mean(tg, n_items_fixated)  %>% transmute(k, stat = "Items fixated at least once (of 4)",  value = fmt(mean, sd)),
  subj_mean(tg %>% filter(n_fix >= 4), all4_in_first4) %>% transmute(k, stat = "All four items in the first four fixations", value = fmt(mean, sd)),
  subj_mean(fg %>% filter(!is_first), is_refixation) %>% transmute(k, stat = "Refixation (item seen before in trial)", value = fmt(mean, sd)),
  subj_mean(tg, first_rank == 1)  %>% transmute(k, stat = "First fixation on the best item",     value = fmt(mean, sd)),
  subj_mean(tg, first_item == 1)  %>% transmute(k, stat = "First fixation on the top item (position)", value = fmt(mean, sd)),
  subj_mean(tg, last_is_chosen)   %>% transmute(k, stat = "Last fixation on a chosen item",      value = fmt(mean, sd)),
  subj_mean(tg, gaze_entropy)     %>% transmute(k, stat = "Gaze entropy (bits, max 2)",          value = fmt(mean, sd)),
  subj_mean(tg, share_r1)         %>% transmute(k, stat = "Dwell share, best item",              value = fmt(mean, sd, 3)),
  subj_mean(tg, share_r4)         %>% transmute(k, stat = "Dwell share, worst item",             value = fmt(mean, sd, 3))
)
tab <- bind_rows(rows) %>% mutate(stat = factor(stat, levels = unique(stat))) %>%
  pivot_wider(names_from = k, values_from = value, names_prefix = "k = ") %>% arrange(stat)
write_csv(tab, here("results/tables/descriptives_by_k.csv"))

# first-fixation position bias (spatial), pooled over k
pos <- tg %>% count(first_item) %>% mutate(p = n / sum(n), position = c("left", "top", "right", "bottom")[first_item + 1])
write_csv(pos, here("results/tables/first_fixation_position.csv"))

cat("| Measure | k = 1 | k = 2 | k = 3 |\n|---|---|---|---|\n")
for (i in seq_len(nrow(tab))) cat(sprintf("| %s | %s | %s | %s |\n", tab$stat[i], tab$`k = 1`[i], tab$`k = 2`[i], tab$`k = 3`[i]))
cat("\nFirst fixation by position:\n"); print(pos)
cat(sprintf("\nSubjects: %d; gaze sampling interval median 53 ms\n", n_distinct(tg$subject_id)))
