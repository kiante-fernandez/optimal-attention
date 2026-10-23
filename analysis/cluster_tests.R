# Cluster-based permutation tests (Maris & Oostenveld, 2007) for the time-resolved value-directed index of Fig 8.
# Index per participant, selection size, and 100 ms bin: P(fixated item is the best) - P(it is the worst) among
# trials with a fixation covering the bin center (>= 5 such trials). One-sample tests against zero and paired
# differences between selection sizes; clusters of adjacent bins with |t| above the two-sided .05 critical value;
# cluster mass = sum of t; null distribution of the largest absolute mass from random sign flips per participant.
# Output: results/tables/cluster_tests.txt
suppressMessages({library(dplyr); library(readr); library(purrr); library(tidyr); library(here)})
set.seed(1); NPERM <- 5000
tr <- read_csv(here("results/data_trials.csv"), show_col_types = FALSE) %>% filter(rt_ok, has_gaze)
fx <- read_csv(here("results/data_fixations.csv"), show_col_types = FALSE) %>% filter(rt_ok) %>% semi_join(tr, by = c("subject_id", "trial"))
bins_s <- seq(50, 2950, by = 100); bins_r <- seq(-2450, -50, by = 100)
index_mat <- function(centers, ref, kk) {
  f <- fx %>% filter(k == kk) %>% select(subject_id, onset, offset, rank, rt_1)
  if (ref == "response") f <- f %>% mutate(onset = onset - rt_1, offset = offset - rt_1)
  d <- map_dfr(centers, function(tc) f %>% filter(onset <= tc, offset > tc) %>% group_by(subject_id) %>%
                 summarise(v = mean(rank == 1) - mean(rank == 4), n = n(), .groups = "drop") %>% mutate(t = tc)) %>% filter(n >= 5)
  d %>% select(subject_id, t, v) %>% complete(subject_id = unique(tr$subject_id), t = centers) %>% arrange(subject_id, t) %>%
    pivot_wider(names_from = t, values_from = v) %>% select(-subject_id) %>% as.matrix()
}
tstat <- function(M) { n <- colSums(!is.na(M)); m <- colMeans(M, na.rm = TRUE); s <- apply(M, 2, sd, na.rm = TRUE); list(t = m / (s / sqrt(n)), crit = qt(0.975, pmax(n - 1, 1))) }
clusters <- function(ts) {
  above <- !is.na(ts$t) & abs(ts$t) > ts$crit; code <- ifelse(above, sign(ts$t), 0)
  r <- rle(code); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1; keep <- which(r$values != 0)
  map(keep, function(i) list(start = starts[i], end = ends[i], mass = sum(ts$t[starts[i]:ends[i]])))
}
cluster_test <- function(M, centers) {
  M <- M[rowSums(!is.na(M)) > 0, , drop = FALSE]; obs <- clusters(tstat(M))
  null <- replicate(NPERM, { cs <- clusters(tstat(M * sample(c(-1, 1), nrow(M), replace = TRUE))); if (length(cs)) max(abs(map_dbl(cs, "mass"))) else 0 })
  tibble(from = centers[map_dbl(obs, "start")] - 50, to = centers[map_dbl(obs, "end")] + 50, mass = map_dbl(obs, "mass"),
         p = map_dbl(obs, function(c) mean(null >= abs(c$mass))), n = nrow(M))
}
report <- function(label, res) {
  cat(sprintf("%s (n = %d participants):\n", label, if (nrow(res)) res$n[1] else NA))
  if (!nrow(res)) cat("  no cluster\n") else for (i in seq_len(nrow(res)))
    cat(sprintf("  %s cluster %d to %d ms, mass %.1f, p %s\n", if (res$mass[i] > 0) "positive" else "negative", res$from[i], res$to[i], res$mass[i], if (res$p[i] < 0.001) "< 0.001" else sprintf("= %.3f", res$p[i])))
}
sink(here("results/tables/cluster_tests.txt"))
cat(sprintf("Cluster-based permutation tests of the time-resolved index (100 ms bins, %d sign-flip permutations, cluster-forming threshold two-sided p < .05 per bin).\n\n", NPERM))
for (ref in c("onset", "response")) {
  centers <- if (ref == "onset") bins_s else bins_r
  M <- map(1:3, ~ index_mat(centers, ref, .x))
  for (kk in 1:3) report(sprintf("%s-aligned, k = %d, index vs 0", ref, kk), cluster_test(M[[kk]], centers))
  report(sprintf("%s-aligned, k = 3 minus k = 1", ref), cluster_test(M[[3]] - M[[1]], centers))
  report(sprintf("%s-aligned, k = 2 minus k = 1", ref), cluster_test(M[[2]] - M[[1]], centers))
  report(sprintf("%s-aligned, k = 3 minus k = 2", ref), cluster_test(M[[3]] - M[[2]], centers))
  cat("\n")
}
sink()
cat(readLines(here("results/tables/cluster_tests.txt")), sep = "\n")
