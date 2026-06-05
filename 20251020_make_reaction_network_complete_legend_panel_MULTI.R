
# ===================================================================
# COMPLETE LEGEND PANEL — MULTI-COLUMN VARIANTS (2 / 3 / 4 COLS)
# Exports:
#   legend_panel_COMPLETE_2COL.svg/.png  (≈7 in × 2.4 in)
#   legend_panel_COMPLETE_3COL.svg/.png  (≈7 in × 1.9 in)
#   legend_panel_COMPLETE_4COL.svg/.png  (≈7 in × 1.6 in)
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
  library(grid)
})

# Reduce console noise about duplicated scales in this synthetic legend
options(ggplot2.discrete.fill = NULL, ggplot2.discrete.colour = NULL)

BASE_TEXT <- 8.4  # adjust for tighter or roomier legends

# ---- Aesthetics (match your figures; S excluded if your main figures exclude S) ----
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666"
)

fill_map_phase <- c(
  "aq" = "#e6f5ff",
  "sorbed" = "#fff2cc",
  "solid/colloid" = "#f2e6ff",
  "microsite" = "#ffe6e6",
  "bulk" = "#e6ffe6"
)

col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
proc_types <- c(solid = 1, dashed = 2, dotted = 3)
shape_map <- c("Mobile (aq)" = 21, "Immobile/Sorbed" = 22, "Other" = 25)

DOC_levels <- c("Low DOC", "Moderate DOC", "High DOC")
DOC_linewidths <- c("Low DOC" = 0.45, "Moderate DOC" = 0.85, "High DOC" = 1.20)

# ---- Dummy canvas to gather all guides ----
edge_df <- expand.grid(
  mobility_effect = names(col_mob),
  mediator_class  = names(proc_types)
) |>
  mutate(x = 0, xend = 1, y = row_number(), yend = y,
         DOC_level = factor(rep(DOC_levels, length.out = n()), levels = DOC_levels))

node_df <- expand.grid(
  mobility_state = names(shape_map),
  phase_norm     = names(fill_map_phase),
  element        = names(stroke_map)
) |>
  mutate(x = 0.5, y = row_number() + max(edge_df$y) + 1)

doc_df <- tibble(
  DOC_level = factor(DOC_levels, levels = DOC_levels),
  x = 0, xend = 1, y = seq_len(length(DOC_levels)) + max(node_df$y) + 1, yend = y
)

p <- suppressWarnings({
  ggplot() +
    geom_segment(
      data = edge_df,
      aes(x = x, y = y, xend = xend, yend = yend,
          color = mobility_effect, linetype = mediator_class, linewidth = DOC_level),
      show.legend = TRUE
    ) +
    scale_color_manual(values = col_mob, name = "Effect on mobility") +
    scale_linetype_manual(values = proc_types, name = "Process type",
                          breaks = c("solid","dashed","dotted"),
                          labels = c("Abiotic","Abiotic (mineral-mediated)","Microbial")) +
    scale_linewidth_manual(values = DOC_linewidths, name = "DOC level") +
    guides(
      linetype = guide_legend(order = 2, override.aes = list(linewidth = 0.9, color = "grey30")),
      color    = guide_legend(order = 1),
      linewidth= guide_legend(order = 3)
    ) +
    geom_point(
      data = node_df,
      aes(x = x, y = y, shape = mobility_state, fill = phase_norm, color = element),
      size = 3, stroke = 0.9, show.legend = TRUE
    ) +
    scale_shape_manual(values = shape_map, name = "Mobility state") +
    scale_fill_manual(values = fill_map_phase, name = "Phase") +
    scale_colour_manual(values = stroke_map, name = "Element") +
    guides(
      shape = guide_legend(order = 4, override.aes = list(color = "grey20")),
      fill  = guide_legend(order = 5, override.aes = list(color = "grey20")),
      colour= guide_legend(order = 6)
    ) +
    geom_segment(
      data = doc_df,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = DOC_level),
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    theme_void(base_size = BASE_TEXT) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.box = "vertical",
      legend.title = element_text(size = BASE_TEXT, face = "bold"),
      legend.text  = element_text(size = BASE_TEXT - 1),
      plot.margin  = margin(2, 6, 2, 6)
    )
})

# ---- Extract ALL guide boxes ----
leg_list <- cowplot::get_plot_component(p, "guide-box", return_all = TRUE)
if (length(leg_list) == 0) stop("No legends found to arrange.")

make_multicol_panel <- function(leg_list, ncol, file_stub, width_in, height_in) {
  n <- length(leg_list)
  rows <- ceiling(n / ncol)
  # pad with null grobs if needed
  if (rows * ncol > n) {
    pad <- rows * ncol - n
    leg_list2 <- c(leg_list, replicate(pad, grid::nullGrob(), simplify = FALSE))
  } else {
    leg_list2 <- leg_list
  }
  grid <- gridExtra::arrangeGrob(grobs = leg_list2, nrow = rows, ncol = ncol,
                                 heights = unit(rep(1, rows), "null"),
                                 widths  = unit(rep(1, ncol), "null"))
  title_g <- textGrob("Legend", gp = gpar(fontface = "bold", cex = 1.05))
  panel <- arrangeGrob(grobs = list(title_g, grid), ncol = 1,
                       heights = unit.c(unit(0.20, "in"), unit(1, "null")))
  ggsave(paste0("legend_panel_COMPLETE_", file_stub, ".svg"), panel, width = width_in, height = height_in)
  ggsave(paste0("legend_panel_COMPLETE_", file_stub, ".png"), panel, width = width_in, height = height_in, dpi = 600)
}

# Export 2/3/4 columns
make_multicol_panel(leg_list, ncol = 2, file_stub = "2COL", width_in = 7, height_in = 2.4)
make_multicol_panel(leg_list, ncol = 3, file_stub = "3COL", width_in = 7, height_in = 1.9)
make_multicol_panel(leg_list, ncol = 4, file_stub = "4COL", width_in = 7, height_in = 1.6)
