# fit_table.R: Table 2 (fitted parameters per policy) and the fitted summary statistics against the data.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(here)})
pols <- c(simultaneous = "Simultaneous", sequential = "Best-first", commit = "Commit when certain")
# Following Callaway et al. (2021), parameters are reported as the mean and SD of the best configurations of the
# search (here the ten best of the refinement stage); the fitted statistics are those of the single best configuration.
best <- bind_rows(lapply(names(pols), function(p) {
  f <- here(sprintf("results/fits/fit_%s_stage2.csv", p)); if (!file.exists(f)) f <- here(sprintf("results/fits/fit_%s_stage1.csv", p))
  if (!file.exists(f)) return(NULL)
  d <- read_csv(f, show_col_types = FALSE) %>% arrange(loss); top <- d %>% slice(1:10)
  d %>% slice(1) %>% mutate(policy = pols[[p]], file = basename(f),
    σ_lab = sprintf("%.2f ± %.2f", mean(top$σ), sd(top$σ)), γs_lab = sprintf("%.4f ± %.4f", mean(top$γs), sd(top$γs)), γw_lab = sprintf("%.3f ± %.3f", mean(top$γw), sd(top$γw)))
}))
tgt <- read_csv(here("results/data_summary_by_k.csv"), show_col_types = FALSE)
tab <- best %>% transmute(Policy = policy, `σ_x` = σ_lab, `γ_sample` = γs_lab, `γ_switch` = γw_lab, Loss = sprintf("%.1f", loss),
                          `Acc. k=1/2/3` = sprintf("%.2f / %.2f / %.2f", acc1, acc2, acc3),
                          `Fixations k=1/2/3` = sprintf("%.1f / %.1f / %.1f", nfix1, nfix2, nfix3),
                          `Fix. time (ms) k=1/2/3` = sprintf("%.0f / %.0f / %.0f", time1, time2, time3),
                          `P(first best) k=2/3` = sprintf("%.2f / %.2f", fib2, fib3), `P(in order) k=2/3` = sprintf("%.2f / %.2f", inord2, inord3),
                          `Time to resp. 1 (ms) k=2/3` = sprintf("%.0f / %.0f", t1_2, t1_3))
data_row <- tibble(Policy = "Data", `σ_x` = "", `γ_sample` = "", `γ_switch` = "", Loss = "",
                   `Acc. k=1/2/3` = sprintf("%.2f / %.2f / %.2f", tgt$accuracy[1], tgt$accuracy[2], tgt$accuracy[3]),
                   `Fixations k=1/2/3` = sprintf("%.1f / %.1f / %.1f", tgt$mean_n_fix[1], tgt$mean_n_fix[2], tgt$mean_n_fix[3]),
                   `Fix. time (ms) k=1/2/3` = sprintf("%.0f / %.0f / %.0f", tgt$mean_total_fix_time[1], tgt$mean_total_fix_time[2], tgt$mean_total_fix_time[3]),
                   `P(first best) k=2/3` = sprintf("%.2f / %.2f", tgt$p_first_resp_is_best[2], tgt$p_first_resp_is_best[3]),
                   `P(in order) k=2/3` = sprintf("%.2f / %.2f", tgt$p_in_rank_order[2], tgt$p_in_rank_order[3]),
                   `Time to resp. 1 (ms) k=2/3` = sprintf("%.0f / %.0f", tgt$mean_fix_time_before_resp1[2], tgt$mean_fix_time_before_resp1[3]))
tab <- bind_rows(data_row, tab)
write_csv(tab, here("results/tables/fit_params.csv"))
cat("| ", paste(names(tab), collapse = " | "), " |\n|", paste(rep("---", ncol(tab)), collapse = "|"), "|\n", sep = "")
for (i in seq_len(nrow(tab))) cat("| ", paste(tab[i, ], collapse = " | "), " |\n", sep = "")
