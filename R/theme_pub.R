## theme_pub.R: the one figure theme for the manuscript (Figs 1-5).
## Helvetica Neue throughout, italic axis titles, black text, no grids, bold capital panel tags.
## Fonts resolve through systemfonts; save with save_png() so ragg renders them.
suppressMessages(library(ggplot2))
PUB_FONT <- "Helvetica Neue"
BLUE <- "#2E86AB"; RED <- "#C0392B"; GREY <- "#7F8C8D"   # preference/posterior; identity/true value; neutral
theme_pub <- function(base_size = 16) {
  theme_classic(base_size = base_size, base_family = PUB_FONT) +
    theme(text = element_text(color = "black"),
          axis.title = element_text(size = base_size + 2, face = "italic", color = "black"),
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.text = element_text(size = base_size, color = "black"),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),
          legend.title = element_text(size = base_size - 2, face = "italic", color = "black"),
          legend.text = element_text(size = base_size - 2, color = "black"),
          legend.background = element_blank(), legend.key = element_blank(),
          panel.background = element_blank(),   # lets in-panel text overflow across facets
          strip.background = element_blank(),
          strip.text = element_text(size = base_size - 1, face = "italic", color = "black"),
          plot.tag = element_text(size = base_size + 6, face = "bold", color = "black"),
          plot.background = element_rect(fill = "white", color = NA))
}
save_png <- function(file, plot, width, height)
  ggsave(file, plot, width = width, height = height, dpi = 300, bg = "white", device = ragg::agg_png)

## ---- additions for the multi-response attention paper ----
POL_COLS  <- c(Data = BLUE, Simultaneous = GREY, `Best-first` = RED, `Commit when certain` = "#E69F00")
RANK_COLS <- c(`1` = "#1B4F72", `2` = BLUE, `3` = "#F0B27A", `4` = RED)
K_LAB     <- ggplot2::labeller(k = function(v) paste0("Choose ", v))
