
# ===================================================================
# CONSOLIDATED LEGEND (single panel) — columns, NO DOC, NO PHASE
# Exports:
#   legend_panel_CONSOLIDATED_NoDOC_NoPHASE_{NCOL}COL.svg/.png
# Customize columns via N_COLS at the top.
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
  library(grid)
})

# ---------- Config ----------
N_COLS   <- 3      # change to 2/3/4 columns as needed
BASE_TEXT <- 8.6   # legend font size
TITLE_CEX <- 1.05  # "Legend" title size multiplier
WIDTH_IN  <- 7     # output width in inches
HEIGHT_IN <- 1.6   # output height in inches (adjust if needed)

# ---------- Aesthetics (match main figures; S may be excluded in your figs) ----------
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666"
)
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
proc_types <- c(solid = 1, dashed = 2, dotted = 3)
shape_map <- c("Mobile (aq)" = 21, "Immobile/Sorbed" = 22, "Other" = 25)

# ---------- Build separate dummy plots for each legend block ----------
# A) Effect on mobility + Process type (edge color + linetype)
edge_df <- expand.grid(
  mobility_effect = names(col_mob),
  mediator_class  = names(proc_types)
) |>
  mutate(x = 0, xend = 1, y = row_number(), yend = y)

p_edges <- ggplot(edge_df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend,
                   color = mobility_effect, linetype = mediator_class),
               linewidth = 0.9, show.legend = TRUE) +
  scale_color_manual(values = col_mob, name = "Effect on mobility") +
  scale_linetype_manual(values = proc_types, name = "Process type",
                        breaks = c("solid","dashed","dotted"),
                        labels = c("Abiotic","Abiotic (mineral-mediated)","Microbial")) +
  guides(
    linetype = guide_legend(order = 2, override.aes = list(color = "grey30", linewidth = 1.0)),
    color    = guide_legend(order = 1)
  ) +
  theme_void(base_size = BASE_TEXT) +
  theme(legend.position = "bottom")

# B) Mobility state + Element (node shape + stroke color)
node_df <- expand.grid(
  mobility_state = names(shape_map),
  element        = names(stroke_map)
) |>
  mutate(x = 0.5, y = row_number())

p_nodes <- ggplot(node_df) +
  geom_point(aes(x = x, y = y, shape = mobility_state, color = element),
             size = 3.2, stroke = 0.9, show.legend = TRUE) +
  scale_shape_manual(values = shape_map, name = "Mobility state") +
  scale_colour_manual(values = stroke_map, name = "Element") +
  guides(
    shape = guide_legend(order = 1, override.aes = list(color = "grey20")),
    colour= guide_legend(order = 2)
  ) +
  theme_void(base_size = BASE_TEXT) +
  theme(legend.position = "bottom")

# ---------- Extract guide boxes from both plots ----------
legs_edges <- cowplot::get_plot_component(p_edges, "guide-box", return_all = TRUE)
legs_nodes <- cowplot::get_plot_component(p_nodes, "guide-box", return_all = TRUE)
leg_list <- c(legs_edges, legs_nodes)

if (length(leg_list) == 0) stop("No legends found to arrange.")

# ---------- Arrange into columns within a single panel ----------
n <- length(leg_list)
ncol <- max(1, as.integer(N_COLS))
nrow <- ceiling(n / ncol)

# pad with empty grobs if needed
if (nrow * ncol > n) {
  pad <- nrow * ncol - n
  leg_list2 <- c(leg_list, replicate(pad, grid::nullGrob(), simplify = FALSE))
} else {
  leg_list2 <- leg_list
}

grid_col <- gridExtra::arrangeGrob(grobs = leg_list2, nrow = nrow, ncol = ncol,
                                   heights = unit(rep(1, nrow), "null"),
                                   widths  = unit(rep(1, ncol), "null"))

# Wrap with centered "Legend" title
title_g <- textGrob("Legend", gp = gpar(fontface = "bold", cex = TITLE_CEX))
panel <- arrangeGrob(grobs = list(title_g, grid_col), ncol = 1,
                     heights = unit.c(unit(0.18, "in"), unit(1, "null")))

# ---------- Save ----------
stub <- paste0("NoDOC_NoPHASE_", ncol, "COL")
ggsave(paste0("legend_panel_CONSOLIDATED_", stub, ".svg"), panel, width = WIDTH_IN, height = HEIGHT_IN)
ggsave(paste0("legend_panel_CONSOLIDATED_", stub, ".png"), panel, width = WIDTH_IN, height = HEIGHT_IN, dpi = 600)

message(sprintf("Saved consolidated legend: %s columns, %gx%g in",
                ncol, WIDTH_IN, HEIGHT_IN))
