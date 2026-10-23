# fig1_fig2.R: Fig 1 (task and belief updating) and Fig 2 (the value of sampling by selection size, and the
# fixation-by-rank profiles of the subset policy). House style from R/theme_pub.R. Run after figures.R.
suppressMessages({library(dplyr); library(readr); library(tidyr); library(purrr); library(ggplot2); library(patchwork); library(here)})
source(here("R/theme_pub.R"))
theme_set(theme_pub(15) + theme(strip.text = element_text(size = 15, face = "italic", hjust = 0), panel.spacing.x = unit(1.4, "lines")))
tagged <- function(p) p + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 22, face = "bold"))
out <- function(p, name, w, h) save_png(here(sprintf("figures/paper/%s.png", name)), p, w, h)
ITEM_COLS <- c(left = "#1B4F72", top = BLUE, right = "#F0B27A", bottom = RED)

# ---------------------------------------------------------------- Fig 1
# A. trial timeline
tl <- tibble(stage = factor(c("Fixation cross\n500 ms", "Four snacks\nchoose k with J, I, L, K", "Keypresses\n1st  2nd  3rd"), levels = c("Fixation cross\n500 ms", "Four snacks\nchoose k with J, I, L, K", "Keypresses\n1st  2nd  3rd")), x = 1:3)
box <- function(cx, cy, w = 0.26, h = 0.26, fill = "grey88", col = "grey30", lwd = 0.5) annotate("rect", xmin = cx - w/2, xmax = cx + w/2, ymin = cy - h/2, ymax = cy + h/2, fill = fill, color = col, linewidth = lwd)
cross <- function(cx) list(annotate("segment", x = cx - 0.07, xend = cx + 0.07, y = 0, yend = 0, linewidth = 0.9), annotate("segment", x = cx, xend = cx, y = -0.07, yend = 0.07, linewidth = 0.9))
items <- function(cx, frames = c()) {
  pos <- tibble(item = c("left", "top", "right", "bottom"), dx = c(-0.33, 0, 0.33, 0), dy = c(0, 0.33, 0, -0.33), key = c("J", "I", "L", "K"))
  c(list(box(cx + pos$dx[1], pos$dy[1]), box(cx + pos$dx[2], pos$dy[2]), box(cx + pos$dx[3], pos$dy[3]), box(cx + pos$dx[4], pos$dy[4])),
    map(seq_len(4), ~ annotate("text", cx + pos$dx[.x], pos$dy[.x], label = pos$key[.x], size = 5, family = PUB_FONT)),
    map(frames, ~ annotate("rect", xmin = cx + pos$dx[.x] - 0.16, xmax = cx + pos$dx[.x] + 0.16, ymin = pos$dy[.x] - 0.16, ymax = pos$dy[.x] + 0.16, fill = NA, color = "#E67E22", linewidth = 1.6)))
}
pA <- ggplot() + annotate("rect", xmin = c(0.5, 1.6, 2.7), xmax = c(1.5, 2.6, 3.7), ymin = -0.55, ymax = 0.55, fill = "white", color = "black", linewidth = 0.5) +
  cross(1) + items(2.1) + items(3.2, frames = c(1, 3)) +
  annotate("text", x = c(1, 2.1, 3.2), y = -0.72, label = c("Fixation cross\n500 ms", "Four snacks; choose k\nwith the keys J, I, L, K", "Chosen items are framed;\nthe trial ends at keypress k"), size = 4.2, family = PUB_FONT, vjust = 1, lineheight = 0.95) +
  annotate("segment", x = c(1.52, 2.62), xend = c(1.58, 2.68), y = 0, yend = 0, arrow = arrow(length = unit(0.12, "cm"), type = "closed"), linewidth = 0.6) +
  coord_equal(xlim = c(0.45, 3.75), ylim = c(-1.05, 0.6), expand = FALSE) + theme_void() + theme(plot.margin = margin(4, 4, 4, 4))

# B. belief updating: three snapshots of the four beliefs
set.seed(7); sigma <- 2.6; true_u <- c(left = 0.9, top = -0.6, right = 0.2, bottom = -1.2)
mu <- rep(0, 4); lam <- rep(1, 4); names(mu) <- names(lam) <- names(true_u)
snap <- function(label, fixated) tibble(label = label, item = names(mu), mu = mu, sd = 1 / sqrt(lam), fixated = !is.na(fixated) & names(mu) == fixated)
samp <- function(it, n) for (i in 1:n) { x <- rnorm(1, true_u[it], sigma); lam[it] <<- lam[it] + sigma^-2; mu[it] <<- (sigma^-2 * x + (lam[it] - sigma^-2) * mu[it]) / lam[it] }
S <- list(snap("Prior", NA)); samp("left", 4); S[[2]] <- snap("After 4 samples of the left item", "left"); samp("right", 4); S[[3]] <- snap("After 4 samples of the right item", "right")
bd <- bind_rows(S) %>% mutate(label = factor(label, levels = c("Prior", "After 4 samples of the left item", "After 4 samples of the right item")),
                              item = factor(item, levels = names(ITEM_COLS))) %>%
  expand_grid(v = seq(-3, 3, by = 0.04)) %>% mutate(dens = dnorm(v, mu, sd))
tv <- tibble(item = factor(names(true_u), levels = names(ITEM_COLS)), u = true_u)
eye <- bd %>% filter(fixated) %>% group_by(label, item) %>% summarise(x = first(mu), y = max(dens) + 0.06, .groups = "drop")
pB <- ggplot(bd, aes(v, dens, color = item)) +
  geom_segment(data = tv, aes(x = u, xend = u, y = -0.02, yend = 0.04, color = item), inherit.aes = FALSE, linewidth = 1.6) +
  geom_line(aes(linewidth = fixated)) + scale_linewidth_manual(values = c(`FALSE` = 0.8, `TRUE` = 2), guide = "none") +
  geom_text(data = eye, aes(x, y, label = "fixated", color = item), size = 4.4, family = PUB_FONT, fontface = "italic", show.legend = FALSE) +
  facet_wrap(~label, nrow = 1) + scale_color_manual(values = ITEM_COLS, name = "Item (position)", labels = c("left", "top", "right", "bottom")) +
  scale_y_continuous(limits = c(-0.02, 0.9), breaks = c(0, 0.4, 0.8)) + labs(x = "Value (SD units); ticks mark the true values", y = "Belief density") +
  theme(legend.position = "right", legend.title = element_text(size = 13, face = "italic"))
out(tagged(pA / pB + plot_layout(heights = c(1, 1.15))), "fig1_task_model", 13, 8.5)

# ---------------------------------------------------------------- Fig 2
pars <- c(sigma = 2.6, gs = 0.0037, gw = 0.00995)
ff <- here("results/fits/fit_simultaneous_stage2.csv")
if (file.exists(ff)) { b <- read_csv(ff, show_col_types = FALSE) %>% arrange(loss) %>% slice(1); pars <- c(sigma = b$σ, gs = b$γs, gw = b$γw) }
cat(sprintf("VOC curves with sigma=%.2f, sample=%.4f, switch=%.4f\n", pars["sigma"], pars["gs"], pars["gw"]))
voi_n <- function(mu, lam, cv, selected, sigma, N) {
  lam_obs <- N / sigma^2; w <- lam_obs / (lam + lam_obs); s <- w * sqrt(1 / lam + 1 / lam_obs); z <- (cv - mu) / s
  if (selected) s * (dnorm(z) + z * pnorm(z)) else s * (dnorm(z) - z * (1 - pnorm(z)))
}
voc_dc <- function(mu, lam, c, k, sigma, gs, gw, focused = c) {
  n <- length(mu); ord <- order(mu); r <- which(ord == c); mus <- mu[ord]
  selected <- r > n - k; cv <- if (selected) mus[n - k] else mus[n - k + 1]
  cost1 <- gs + ifelse(focused != 0 && focused != c, gw, 0)
  o <- optimize(function(N) -(voi_n(mu[c], lam[c], cv, selected, sigma, N) - cost1 - (N - 1) * gs), c(1, 100)); -o$objective
}
others_mu <- c(0.6, 0, -0.6)
curves <- expand_grid(k = 1:3, lam1 = c(1, 2, 4), mu1 = seq(-2, 2, by = 0.04)) %>%
  mutate(voc = pmap_dbl(list(k, mu1, lam1), function(k, mu1, lam1) voc_dc(c(mu1, others_mu), c(lam1, rep(4, 3)), 1, k, pars["sigma"], pars["gs"], pars["gw"], focused = 1)))
pA <- ggplot(curves, aes(mu1, voc, color = factor(lam1))) + geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = others_mu, linetype = "dotted", color = "grey50") + geom_line(linewidth = 1.2) +
  facet_wrap(~k, labeller = K_LAB, nrow = 1) + scale_color_manual(values = c(`1` = "#F0B27A", `2` = BLUE, `4` = "#1B4F72"), name = "Precision of the estimate") +
  labs(x = "Estimated value of the sampled item (SD units); dotted lines: the other items' estimates", y = "Value of sampling the item (SD units)") +
  theme(legend.position = "right", legend.title = element_text(size = 13, face = "italic"))
prof <- read_csv(here("results/tables/fixation_rank_profile.csv"), show_col_types = FALSE) %>% filter(source == "Simultaneous")
pB <- ggplot(prof, aes(fix_num, p, color = factor(rank))) + geom_hline(yintercept = 0.25, linetype = "dashed", linewidth = 0.4) + geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
  facet_wrap(~k, labeller = K_LAB, nrow = 1) + scale_color_manual(values = RANK_COLS, name = "Rank of the fixated item") + scale_x_continuous(breaks = 1:6) +
  labs(x = "Fixation number", y = "P(fixation on item of this rank)") + theme(legend.position = "right", legend.title = element_text(size = 13, face = "italic"))
out(tagged(pA / pB), "fig2_policy", 13, 9)
cat("fig1_task_model.png and fig2_policy.png written\n")
