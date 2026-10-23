# policy_cost_table.R: net expected reward of each response policy on the empirical value sets, under
# each parameter set (Callaway et al. 2021 values and each policy's fitted values). Writes
# results/tables/policy_cost_summary.csv and prints a markdown table.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(here)})
files <- list.files(here("results/tables"), pattern = "^policy_cost_.*\\.csv$", full.names = TRUE)
lab <- c(policy_cost_callaway = "Callaway et al. (2021)", policy_cost_fit_simultaneous_stage2 = "fitted, simultaneous",
         policy_cost_fit_sequential_stage2 = "fitted, best-first", policy_cost_fit_commit_stage2 = "fitted, commit-when-certain")
d <- bind_rows(lapply(files, function(f) read_csv(f, show_col_types = FALSE) %>% mutate(set = lab[tools::file_path_sans_ext(basename(f))]))) %>%
  filter(label == "empirical value sets (standardized ratings)", !grepl("superseded", set)) %>%
  mutate(policy = recode(policy, simultaneous = "Simultaneous", sequential = "Best-first", commit = "Commit when certain"))
wide <- d %>% select(set, k, policy, net, oracle) %>% pivot_wider(names_from = policy, values_from = net) %>%
  mutate(`Best-first vs simultaneous` = sprintf("%+.0f%%", 100 * (`Best-first` - Simultaneous) / Simultaneous),
         `Commit vs simultaneous` = sprintf("%+.0f%%", 100 * (`Commit when certain` - Simultaneous) / Simultaneous)) %>%
  arrange(set, k)
write_csv(wide, here("results/tables/policy_cost_summary.csv"))
cat("| Parameter set | k | Oracle | Simultaneous | Best-first | Commit when certain | Best-first vs sim. | Commit vs sim. |\n|---|---|---|---|---|---|---|---|\n")
for (i in seq_len(nrow(wide))) with(wide[i, ], cat(sprintf("| %s | %d | %.3f | %.3f | %.3f | %.3f | %s | %s |\n", set, k, oracle, Simultaneous, `Best-first`, `Commit when certain`, `Best-first vs simultaneous`, `Commit vs simultaneous`)))
