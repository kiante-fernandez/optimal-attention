# figures.R: manuscript figures 3-9 in the house style (R/theme_pub.R). Data: blue points with 95%
# bootstrap CIs over participants (odd-numbered trials); policies simulated on the same trials at
# their fitted parameters: lines. Run after build_data.R and simulations/run_best.sh.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(purrr); library(ggplot2); library(patchwork); library(here)})
source(here("R/theme_pub.R"))
theme_set(theme_pub(15) + theme(legend.position = "bottom", legend.title = element_blank(),
                                 strip.text = element_text(size = 15, face = "italic", hjust = 0),
                                 panel.spacing.x = unit(1.4, "lines"), panel.spacing.y = unit(1.2, "lines")))
MS <- 100
POLICIES <- c(simultaneous = "Simultaneous", sequential = "Best-first", commit = "Commit when certain")
tagged <- function(p) p + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 22, face = "bold"))
noleg <- theme(legend.position = "none")   # one legend per figure: keep it on a single plot_dm panel
out <- function(p, name, w = 13, h = 9) save_png(here(sprintf("figures/paper/%s.png", name)), p, w, h)

# ------------------------------------------------------------------ load and harmonize (same as before)
ODD_ONLY <- TRUE
trials_d <- read_csv(here("results/data_trials.csv"), show_col_types = FALSE) %>% filter(rt_ok, has_gaze, !ODD_ONLY | trial %% 2 == 1) %>%
  mutate(gap_z = boundary_gap / rating_sd, mean_z = (u_0 + u_1 + u_2 + u_3) / 4, source = "Data", rep = 1L,
         rt_2 = ifelse(k >= 2, rt_2, NA), rt_3 = ifelse(k >= 3, rt_3, NA))
info <- trials_d %>% select(subject_id, trial, k, gap_z, mean_z, item_rank_0:item_rank_3, u_0:u_3)
fix_d <- read_csv(here("results/data_fixations.csv"), show_col_types = FALSE) %>%
  semi_join(trials_d, by = c("subject_id", "trial")) %>% mutate(source = "Data", rep = 1L) %>%
  select(source, subject_id, trial, k, rep, fix_num, n_fix, item, onset, offset, duration, value_z, rank, chosen_at, is_chosen)
tr_d <- trials_d %>% select(source, subject_id, trial, k, rep, gap_z, mean_z, total_rt, rt_1, rt_2, rt_3,
                            choice_1, choice_2, choice_3, choice_rank_1, choice_rank_2, choice_rank_3, correct, first_is_best, in_rank_order)
rank_of <- function(ch, r0, r1, r2, r3) case_when(ch == 0 ~ r0, ch == 1 ~ r1, ch == 2 ~ r2, ch == 3 ~ r3)
load_model <- function(pol) {
  ft <- here(sprintf("results/model_%s_trials.csv", pol)); if (!file.exists(ft)) return(NULL)
  lab <- POLICIES[[pol]]
  tr <- read_csv(ft, show_col_types = FALSE) %>% inner_join(info, by = c("subject_id", "trial", "k")) %>%
    mutate(source = lab, total_rt = total_samples * MS, rt_1 = resp_1 * MS, rt_2 = resp_2 * MS, rt_3 = resp_3 * MS,
           choice_rank_1 = rank_of(choice_1, item_rank_0, item_rank_1, item_rank_2, item_rank_3),
           choice_rank_2 = rank_of(choice_2, item_rank_0, item_rank_1, item_rank_2, item_rank_3),
           choice_rank_3 = rank_of(choice_3, item_rank_0, item_rank_1, item_rank_2, item_rank_3),
           in_rank_order = case_when(k == 1 ~ NA, k == 2 ~ choice_rank_1 < choice_rank_2, TRUE ~ choice_rank_1 < choice_rank_2 & choice_rank_2 < choice_rank_3)) %>%
    select(all_of(names(tr_d)))
  fx <- read_csv(here(sprintf("results/model_%s_fixations.csv", pol)), show_col_types = FALSE) %>%
    semi_join(trials_d, by = c("subject_id", "trial")) %>%
    mutate(source = lab, onset = onset * MS, duration = duration * MS, offset = onset + duration, is_chosen = !is.na(chosen_at)) %>%
    select(all_of(names(fix_d)))
  list(tr = tr, fx = fx)
}
models <- compact(map(names(POLICIES), load_model))
tr <- bind_rows(tr_d, map(models, "tr")); fx <- bind_rows(fix_d, map(models, "fx"))
tr$source <- factor(tr$source, levels = names(POL_COLS)); fx$source <- factor(fx$source, levels = names(POL_COLS))
cat("sources:", paste(levels(droplevels(fx$source)), collapse = ", "), "\n")

fx <- fx %>% inner_join(tr %>% select(source, subject_id, trial, rep, k, total_rt, rt_1, rt_2, rt_3), by = c("source", "subject_id", "trial", "rep", "k")) %>%
  group_by(source, subject_id, trial, rep) %>% arrange(onset, .by_group = TRUE) %>%
  mutate(is_first = fix_num == 1, is_last = fix_num == n_fix,
         phase = case_when(onset < rt_1 ~ 1L, k >= 2 & onset < rt_2 ~ 2L, k == 3 & onset < rt_3 ~ 3L, TRUE ~ k + 1L),
         prev_item = lag(item), is_refixation = map2_lgl(fix_num, item, ~ .y %in% item[seq_len(.x - 1)]),
         cum_total = c(0, cumsum(duration))[fix_num],
         cum_self = map_dbl(fix_num, function(i) sum(duration[seq_len(i - 1)][item[seq_len(i - 1)] == item[i]])),
         fix_adv = cum_self - (cum_total - cum_self) / 3,
         least_dwelled = map_lgl(fix_num, function(i) {
           if (i == 1) return(NA); cd <- sapply(0:3, function(j) sum(duration[seq_len(i - 1)][item[seq_len(i - 1)] == j]))
           alt <- setdiff(0:3, item[i - 1]); m <- min(cd[alt + 1]); sum(cd[alt + 1] == m) == 1 && cd[item[i] + 1] == m })) %>%
  ungroup()
gaze_trial <- fx %>% group_by(source, subject_id, trial, rep, k) %>%
  summarise(n_fix = n(), total_fix_time = sum(duration), n_items = n_distinct(item), all4_in_first4 = n() >= 4 && n_distinct(item[1:4]) == 4,
            first_rank = rank[1], first_duration = duration[1], last_is_chosen = is_chosen[n()],
            share_r1 = sum(duration[rank == 1]) / sum(duration), share_r2 = sum(duration[rank == 2]) / sum(duration),
            share_r3 = sum(duration[rank == 3]) / sum(duration), share_r4 = sum(duration[rank == 4]) / sum(duration), .groups = "drop") %>%
  mutate(entropy = pmap_dbl(list(share_r1, share_r2, share_r3, share_r4), function(...) { p <- c(...); p <- p[p > 0]; -sum(p * log2(p)) }))
tr <- tr %>% inner_join(gaze_trial, by = c("source", "subject_id", "trial", "rep", "k"))

# ------------------------------------------------------------------ helpers
boot_ci <- function(v, n = 500) { v <- v[!is.na(v)]; if (length(v) < 3) return(c(NA, NA)); m <- replicate(n, mean(sample(v, replace = TRUE))); quantile(m, c(.025, .975)) }
summ <- function(df, y, ...) {
  df %>% group_by(source, subject_id, ...) %>% summarise(v = mean({{ y }}, na.rm = TRUE), .groups = "drop") %>%
    group_by(across(-c(subject_id, v))) %>% summarise(mean = mean(v, na.rm = TRUE), lo = boot_ci(v)[1], hi = boot_ci(v)[2], n = n(), .groups = "drop")
}
pol_scale <- function() scale_color_manual(values = POL_COLS, breaks = names(POL_COLS), drop = FALSE,
  guide = guide_legend(override.aes = list(shape = c(16, NA, NA, NA), linetype = c(0, 1, 1, 1), linewidth = 1.1, size = 3)))
plot_dm <- function(s, x, ylab, xlab, facet = TRUE, hline = NULL) {
  p <- ggplot() +
    geom_line(data = filter(s, source != "Data"), aes({{ x }}, mean, color = source, group = source), linewidth = 1.1) +
    geom_errorbar(data = filter(s, source == "Data"), aes({{ x }}, ymin = lo, ymax = hi, color = source), width = 0, linewidth = 0.6) +
    geom_point(data = filter(s, source == "Data"), aes({{ x }}, mean, color = source), size = 2.8) +
    pol_scale() + labs(x = xlab, y = ylab)
  if (!is.null(hline)) p <- p + geom_hline(yintercept = hline, linetype = "dashed", color = "black", linewidth = 0.4)
  if (facet) p <- p + facet_wrap(~k, labeller = K_LAB, nrow = 1)
  p
}
gap_bins <- c(-0.01, 0.15, 0.3, 0.5, 0.8, 1.2, 10); gap_mid <- function(g) { b <- cut(g, gap_bins); ave(g, b, FUN = function(x) round(mean(x), 2)) }
tr <- tr %>% mutate(gap_bin = gap_mid(gap_z), mean_bin = cut(mean_z, c(-10, -1, -0.5, 0, 0.5, 1, 10), labels = c(-1.25, -0.75, -0.25, 0.25, 0.75, 1.25)) %>% as.character() %>% as.numeric())

# ------------------------------------------------------------------ Fig 3: psychometrics
pA <- plot_dm(summ(tr, correct, k, gap_bin), gap_bin, "P(chose the top-k set)", "Gap at the selection boundary (SD units)")
dens <- tr %>% filter(total_fix_time < 6000) %>% group_by(source, k) %>% group_modify(~ { d <- density(.x$total_fix_time, from = 0, to = 6000, n = 121, bw = 200); tibble(x = d$x, y = d$y * 1000) })
pB <- ggplot(dens, aes(x / 1000, y, color = source)) + geom_line(linewidth = 1.1) + facet_wrap(~k, labeller = K_LAB, nrow = 1) +
  pol_scale() + labs(x = "Total fixation time (s)", y = "Density")
pC <- plot_dm(summ(tr, total_fix_time / 1000, k, gap_bin), gap_bin, "Total fixation time (s)", "Gap at the selection boundary (SD units)")
pD <- plot_dm(summ(tr, total_fix_time / 1000, k, mean_bin), mean_bin, "Total fixation time (s)", "Mean item value (SD units)") + scale_x_continuous(breaks = c(-1, 0, 1))
out(tagged((pA + pB) / (pC + pD) + plot_layout(guides = "collect")), "fig3_psychometrics")

# ------------------------------------------------------------------ Fig 4: basic fixation patterns
hist_s <- tr %>% mutate(nf = pmin(n_fix, 8)) %>% group_by(source, subject_id, k) %>% count(nf) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  complete(nesting(source, subject_id, k), nf = 1:8, fill = list(p = 0)) %>% group_by(source, k, nf) %>% summarise(mean = mean(p), lo = boot_ci(p)[1], hi = boot_ci(p)[2], .groups = "drop")
pA <- ggplot() + geom_col(data = filter(hist_s, source == "Data"), aes(nf, mean), fill = BLUE, alpha = 0.35, width = 0.8) +
  geom_errorbar(data = filter(hist_s, source == "Data"), aes(nf, ymin = lo, ymax = hi), width = 0, linewidth = 0.6, color = BLUE) +
  geom_line(data = filter(hist_s, source != "Data"), aes(nf, mean, color = source), linewidth = 1.1) +
  facet_wrap(~k, labeller = K_LAB, nrow = 1) + pol_scale() +
  scale_x_continuous(breaks = 1:8, labels = c(1:7, "8+")) + labs(x = "Number of fixations", y = "Proportion of trials")
pB <- plot_dm(summ(tr, n_fix, k, gap_bin), gap_bin, "Number of fixations", "Gap at the selection boundary (SD units)")
fxo <- fx %>% mutate(ord = case_when(is_last ~ "final", fix_num <= 5 ~ as.character(fix_num), TRUE ~ ">5"), ord = factor(ord, levels = c("1", "2", "3", "4", "5", ">5", "final"))) %>% filter(n_fix >= 2)
pC <- plot_dm(summ(fxo, duration, k, ord), ord, "Fixation duration (ms)", "Fixation number")
out(tagged((pA + noleg + pB) / (pC + noleg) + plot_layout(guides = "collect")), "fig4_fixation_patterns")

# ------------------------------------------------------------------ Fig 5: uncertainty-directed attention
adv_breaks <- c(-Inf, -1500, -1000, -500, -200, 0, 200, 500, 1000, 1500, Inf)
adv_labs <- c("< -1.5", "-1.5 to -1", "-1 to -0.5", "-0.5 to -0.2", "-0.2 to 0", "0 to 0.2", "0.2 to 0.5", "0.5 to 1", "1 to 1.5", "> 1.5")
adv <- fx %>% filter(!is_first) %>% mutate(adv_bin = cut(fix_adv, adv_breaks, labels = adv_labs)) %>%
  group_by(source, subject_id, k) %>% count(adv_bin) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  complete(nesting(source, subject_id, k), adv_bin, fill = list(p = 0)) %>% group_by(source, k, adv_bin) %>%
  summarise(mean = mean(p), lo = boot_ci(p)[1], hi = boot_ci(p)[2], .groups = "drop")
pA <- ggplot() + geom_col(data = filter(adv, source == "Data"), aes(adv_bin, mean), fill = BLUE, alpha = 0.35) +
  geom_errorbar(data = filter(adv, source == "Data"), aes(adv_bin, ymin = lo, ymax = hi), width = 0, linewidth = 0.6, color = BLUE) +
  geom_line(data = filter(adv, source != "Data"), aes(adv_bin, mean, color = source, group = source), linewidth = 1.1) +
  facet_wrap(~k, labeller = K_LAB, nrow = 1) + pol_scale() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) + labs(x = "Fixation advantage at fixation onset (s)", y = "Proportion of fixations")
pB <- plot_dm(summ(fx %>% filter(fix_num >= 2), least_dwelled, k), k, "P(fixate the least-viewed alternative)", "Selection size", facet = FALSE, hline = 1/3) + scale_x_continuous(breaks = 1:3)
pC <- plot_dm(summ(fx %>% filter(!is_first), is_refixation, k, ord = pmin(fix_num, 6)), ord, "P(refixation)", "Fixation number") + scale_x_continuous(breaks = 2:6, labels = c(2:5, "6+"))
out(tagged((pA + noleg + pB + plot_layout(widths = c(3, 1))) / (pC + noleg) + plot_layout(guides = "collect")), "fig5_uncertainty")

# ------------------------------------------------------------------ Fig 6: value-directed attention
share_l <- tr %>% select(source, subject_id, k, trial, rep, share_r1:share_r4) %>% pivot_longer(share_r1:share_r4, names_to = "rank", values_to = "share", names_prefix = "share_r") %>% mutate(rank = as.integer(rank))
pA <- plot_dm(summ(share_l, share, k, rank), rank, "Share of fixation time", "Item rank (1 = best)", hline = 0.25)
pB <- plot_dm(summ(tr, first_duration, k, first_rank), first_rank, "First fixation duration (ms)", "Rank of first-fixated item")
fxt <- fx %>% filter(onset >= 0, onset < total_rt) %>% mutate(dec = pmin(floor(onset / total_rt * 5) + 1, 5))
vdi <- fxt %>% group_by(source, subject_id, k, dec) %>% summarise(v = mean(rank == 1) - mean(rank == 4), .groups = "drop") %>%
  group_by(source, k, dec) %>% summarise(mean = mean(v), lo = boot_ci(v)[1], hi = boot_ci(v)[2], .groups = "drop")
pC <- plot_dm(vdi, dec, "P(fixate best) - P(fixate worst)", "Time in trial (quintile)", hline = 0)
pD <- plot_dm(summ(tr, entropy, k, gap_bin), gap_bin, "Gaze entropy (bits)", "Gap at the selection boundary (SD units)")
out(tagged((pA + pB) / (pC + pD) + plot_layout(guides = "collect")), "fig6_value_directed")

# ------------------------------------------------------------------ Fig 7: response phases and response order
ph <- fx %>% filter(k >= 2, phase <= k) %>% group_by(source, subject_id, trial, rep, k, phase) %>%
  summarise(tot = sum(duration), r1 = sum(duration[rank == 1]), r2 = sum(duration[rank == 2]), r3 = sum(duration[rank == 3]), r4 = sum(duration[rank == 4]), .groups = "drop") %>%
  pivot_longer(r1:r4, names_to = "rank", values_to = "d", names_prefix = "r") %>% mutate(share = d / tot, rank = as.integer(rank)) %>%
  group_by(source, subject_id, k, phase, rank) %>% summarise(v = mean(share), .groups = "drop") %>%
  group_by(source, k, phase, rank) %>% summarise(mean = mean(v), lo = boot_ci(v)[1], hi = boot_ci(v)[2], .groups = "drop") %>%
  mutate(phase_lab = factor(paste0("Before response ", phase), levels = paste0("Before response ", 1:3)))
pA <- ggplot() + geom_line(data = filter(ph, source != "Data"), aes(rank, mean, color = source), linewidth = 1.1) +
  geom_errorbar(data = filter(ph, source == "Data"), aes(rank, ymin = lo, ymax = hi, color = source), width = 0, linewidth = 0.6) +
  geom_point(data = filter(ph, source == "Data"), aes(rank, mean, color = source), size = 2.8) +
  geom_hline(yintercept = 0.25, linetype = "dashed", linewidth = 0.4) + facet_grid(k ~ phase_lab, labeller = labeller(k = function(v) paste0("Choose ", v))) +
  pol_scale() + labs(x = "Item rank (1 = best)", y = "Share of fixation time within phase")
chosen_fix <- fx %>% filter(k >= 2, phase >= 2, phase <= k) %>% mutate(already = !is.na(chosen_at) & chosen_at < phase)
pB <- plot_dm(summ(chosen_fix, already, k), k, "P(fixate an already-chosen item)", "Selection size", facet = FALSE, hline = 0.25) + scale_x_continuous(breaks = 2:3)
irt <- tr %>% filter(k >= 2) %>% transmute(source, subject_id, k, i1 = rt_1, i2 = rt_2 - rt_1, i3 = rt_3 - rt_2) %>%
  pivot_longer(i1:i3, names_to = "resp", values_to = "irt", names_prefix = "i") %>% mutate(resp = as.integer(resp)) %>% filter(!is.na(irt), resp <= k) %>%
  group_by(source, subject_id, k, resp) %>% summarise(v = median(irt), .groups = "drop") %>% group_by(source, k, resp) %>%
  summarise(mean = mean(v) / 1000, lo = boot_ci(v)[1] / 1000, hi = boot_ci(v)[2] / 1000, .groups = "drop")
pC <- plot_dm(irt, resp, "Time since previous response (s, median)", "Response number") + scale_x_continuous(breaks = 1:3)
ro <- tr %>% filter(k >= 2) %>% group_by(source, subject_id, k) %>% count(choice_rank_1) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  complete(nesting(source, subject_id, k), choice_rank_1 = 1:4, fill = list(p = 0)) %>% group_by(source, k, choice_rank_1) %>%
  summarise(mean = mean(p), lo = boot_ci(p)[1], hi = boot_ci(p)[2], .groups = "drop")
pD <- plot_dm(ro, choice_rank_1, "P(first response)", "Rank of the item chosen first")
out(tagged((pA + noleg) / (pB + noleg + pC + noleg + pD + plot_layout(widths = c(1, 2, 2))) + plot_layout(guides = "collect", heights = c(2, 1.3))), "fig7_response_phases", 13, 11)

# ------------------------------------------------------------------ Fig 8: time-resolved fixation probability
bins_s <- seq(50, 2950, by = 100); bins_r <- seq(-2450, -50, by = 100)
time_resolved <- function(centers, ref = c("onset", "response")) {
  f <- fx %>% select(source, subject_id, trial, rep, k, onset, offset, rank, rt_1)
  if (ref == "response") f <- f %>% mutate(onset = onset - rt_1, offset = offset - rt_1)
  map_dfr(centers, function(tc) f %>% filter(onset <= tc, offset > tc) %>% group_by(source, subject_id, k) %>%
            summarise(v = mean(rank == 1) - mean(rank == 4), n = n(), .groups = "drop") %>% mutate(t = tc)) %>%
    filter(n >= 5) %>% group_by(source, k, t) %>% summarise(mean = mean(v), lo = boot_ci(v, 200)[1], hi = boot_ci(v, 200)[2], .groups = "drop")
}
ts <- time_resolved(bins_s, "onset"); trr <- time_resolved(bins_r, "response")
plot_tr <- function(s, xlab) ggplot() +
  geom_ribbon(data = filter(s, source == "Data"), aes(t / 1000, ymin = lo, ymax = hi), fill = BLUE, alpha = 0.25) +
  geom_line(data = filter(s, source == "Data"), aes(t / 1000, mean, color = source), linewidth = 1.3) +
  geom_line(data = filter(s, source != "Data"), aes(t / 1000, mean, color = source), linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) + facet_wrap(~k, labeller = K_LAB, nrow = 1) +
  scale_color_manual(values = POL_COLS, breaks = names(POL_COLS), drop = FALSE) + labs(x = xlab, y = "P(fixate best) - P(fixate worst)")
out(tagged(plot_tr(ts, "Time from stimulus onset (s)") / plot_tr(trr, "Time to first response (s)") + plot_layout(guides = "collect")), "fig8_time_resolved", 13, 9)

# ------------------------------------------------------------------ Fig 9: attention and choice
item_l <- fx %>% group_by(source, subject_id, trial, rep, k, item, rank) %>% summarise(d = sum(duration), chosen = any(is_chosen), .groups = "drop") %>%
  group_by(source, subject_id, trial, rep) %>% mutate(share = d / sum(d)) %>% ungroup() %>%
  mutate(share_bin = cut(share, c(-0.01, 0.1, 0.25, 0.4, 0.6, 1), labels = c(0.05, 0.175, 0.325, 0.5, 0.8)) %>% as.character() %>% as.numeric())
cs <- item_l %>% group_by(source, subject_id, k, rank, share_bin) %>% summarise(v = mean(chosen), .groups = "drop") %>%
  group_by(source, k, rank, share_bin) %>% summarise(mean = mean(v), lo = boot_ci(v, 200)[1], hi = boot_ci(v, 200)[2], n = n(), .groups = "drop") %>% filter(n >= 10)
pA <- ggplot() + geom_line(data = filter(cs, source == "Best-first"), aes(share_bin, mean, color = factor(rank)), linewidth = 1.1) +
  geom_errorbar(data = filter(cs, source == "Data"), aes(share_bin, ymin = lo, ymax = hi, color = factor(rank)), width = 0, linewidth = 0.6) +
  geom_point(data = filter(cs, source == "Data"), aes(share_bin, mean, color = factor(rank)), size = 2.8) +
  facet_wrap(~k, labeller = K_LAB, nrow = 1) + scale_color_manual(values = RANK_COLS, name = "Item rank") +
  labs(x = "Share of fixation time on the item", y = "P(item chosen)") + theme(legend.position = "right", legend.title = element_text(size = 13, face = "italic"))
last <- fx %>% filter(is_last) %>% inner_join(info %>% select(subject_id, trial, u_0:u_3), by = c("subject_id", "trial")) %>%
  mutate(rel = value_z - (u_0 + u_1 + u_2 + u_3 - value_z) / 3, rel_bin = cut(rel, c(-10, -1, -0.5, 0, 0.5, 1, 10), labels = c(-1.25, -0.75, -0.25, 0.25, 0.75, 1.25)) %>% as.character() %>% as.numeric())
pB <- plot_dm(summ(last, is_chosen, k, rel_bin), rel_bin, "P(last-fixated item chosen)", "Relative value of last-fixated item (SD units)") + scale_x_continuous(breaks = c(-1, 0, 1))
first <- fx %>% filter(is_first, n_fix >= 2) %>% group_by(source, subject_id) %>% mutate(dur_q = ntile(duration, 3)) %>% ungroup()
pC <- plot_dm(summ(first, is_chosen, k, dur_q), dur_q, "P(first-fixated item chosen)", "First fixation duration (tercile)") + scale_x_continuous(breaks = 1:3)
out(tagged(pA / (pB + pC + noleg) + plot_layout(guides = "collect")), "fig9_choice", 13, 9.5)

# ------------------------------------------------------------------ tables for the text
write_csv(summ(share_l, share, k, rank), here("results/tables/fig6a_share_by_rank.csv")); write_csv(ph, here("results/tables/fig7a_share_by_phase.csv"))
write_csv(irt, here("results/tables/fig7c_inter_response_times.csv")); write_csv(ro, here("results/tables/fig7d_first_response_rank.csv"))
write_csv(tr %>% group_by(source, k) %>% summarise(across(c(correct, first_is_best, in_rank_order, n_fix, total_fix_time, entropy, share_r1, share_r4, last_is_chosen), ~ mean(.x, na.rm = TRUE)), .groups = "drop"), here("results/tables/summary_by_source_k.csv"))
# rank-by-fixation profiles of the simultaneous policy, for Fig 2
fx %>% filter(fix_num <= 6) %>% group_by(source, k, fix_num, rank) %>% summarise(n = n(), .groups = "drop") %>% group_by(source, k, fix_num) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  write_csv(here("results/tables/fixation_rank_profile.csv"))
cat("figures written to figures/paper/\n")
