
# ===================================================================
# CONSOLIDATED LEGEND — ULTRA-COMPACT, AUTO-FIT (NO DOC, NO PHASE)
# Exports: legend_panel_CONSOLIDATED_UltraCompact.svg/.png
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
  library(grid)
})

PANEL_WIDTH  <- 7.0
PANEL_HEIGHT <- 1.6

# Try tighter first: more columns, small base text
CANDIDATE_COLS <- c(5, 4, 3, 2)
CANDIDATE_TEXT <- c(8.0, 7.6, 7.2, 6.8, 6.5)

TITLE_CEX <- 1.0

# Aesthetics (match main figs; S excluded upstream if needed)
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666"
)
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
proc_types <- c(solid = 1, dashed = 2, dotted = 3)
shape_map <- c("Mobile (aq)" = 21, "Immobile" = 22, "Other" = 25)  # shortened "Immobile"

# ---- Compact "edges" legend (effect + process) with abbreviated labels ----
make_edge_plot <- function(base_text) {
  edge_df <- expand.grid(
    mobility_effect = factor(c("mobilize","immobilize","neutral"), levels=c("mobilize","immobilize","neutral")),
    mediator_class  = factor(c("solid","dashed","dotted"), levels=c("solid","dashed","dotted"))
  ) |>
    mutate(x = 0, xend = 1, y = row_number(), yend = y)
  ggplot(edge_df) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend,
                     color = mobility_effect, linetype = mediator_class),
                 linewidth = 0.9, show.legend = TRUE) +
    scale_color_manual(values = col_mob, name = "Effect",
                       breaks = c("mobilize","immobilize","neutral"),
                       labels = c("Mobilize","Immobilize","Neutral")) +
    scale_linetype_manual(values = proc_types, name = "Process",
                          breaks = c("solid","dashed","dotted"),
                          labels = c("Abiotic","Mineral-med.","Microbial")) +
    guides(
      color = guide_legend(
        order = 1,
        keyheight = unit(6, "pt"),
        keywidth  = unit(10, "pt"),
        label.hjust = 0
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = "grey20",
          linewidth = 1.4,
          linetype = c("solid","22","12")  # solid, longer dashes, shorter dashes
        ),
        keyheight = unit(6, "pt"),
        keywidth  = unit(14, "pt"),
        label.hjust = 0
      )
    ) +
    theme_void(base_size = base_text) +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(0,0,0,0),
      legend.margin = margin(0,0,0,0),
      legend.text = element_text(margin=margin(l=1, r=2)),
      legend.key.height = unit(6, "pt"),
      legend.key.width  = unit(8, "pt")
    )
}

# ---- Compact "nodes" legend (mobility state) ----
make_node_plot <- function(base_text) {
  node_df <- data.frame(mobility_state = factor(names(shape_map), levels = names(shape_map)), x=0.5, y=1:3)
  ggplot(node_df) +
    geom_point(aes(x = x, y = y, shape = mobility_state),
               size = 2.8, stroke = 0.9, color = "grey25", fill = NA, show.legend = TRUE) +
    scale_shape_manual(values = shape_map, name = "State") +
    guides(shape = guide_legend(order = 1, keyheight = unit(6, "pt"), keywidth = unit(8, "pt"))) +
    theme_void(base_size = base_text) +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(0,0,0,0),
      legend.margin = margin(0,0,0,0),
      legend.key.height = unit(6, "pt"),
      legend.key.width  = unit(8, "pt")
    )
}

extract_all_guides <- function(p) cowplot::get_plot_component(p, "guide-box", return_all = TRUE)

# ---- Build a compact Element swatch grid (points with stroke colors + labels), no ggplot legend ----
make_element_grid <- function(base_text, ncol = 6) {
  els <- names(stroke_map)
  els <- els[els != "default"]
  n <- length(els)
  rows <- ceiling(n / ncol)
  # build grobs
  grob_list <- list()
  idx <- 1
  for (r in seq_len(rows)) {
    row_grobs <- list()
    for (c in seq_len(ncol)) {
      if (idx <= n) {
        el <- els[idx]
        # point + label grob
        pt <- pointsGrob(x = unit(0, "npc") + unit(6, "pt"), y = unit(0.5, "npc"),
                         pch = 21, size = unit(2.8, "mm"),
                         gp = gpar(col = stroke_map[[el]], fill = NA, lwd = 1))
        lb <- textGrob(el, x = unit(0, "npc") + unit(14, "pt"), y = unit(0.5, "npc"),
                       just = c("left","center"),
                       gp = gpar(cex = base_text/10))  # approx scaling
        cell <- grobTree(pt, lb)
      } else {
        cell <- nullGrob()
      }
      row_grobs[[c]] <- gTree(children = gList(cell),
                              vp = viewport(width = unit(1,"null"), height = unit(1,"null"),
                                            x = 0, just = c("left","center")))
      idx <- idx + 1
    }
    grob_list[[r]] <- arrangeGrob(grobs = row_grobs, ncol = ncol,
                                  widths = unit(rep(1, ncol), "null"))
  }
  # title + grid
  title <- textGrob("Element", gp = gpar(fontface = "bold", cex = 0.95))
  grid <- arrangeGrob(grobs = grob_list, ncol = 1,
                      heights = unit(rep(1, rows), "null"))
  arrangeGrob(title, grid, ncol = 1, heights = unit.c(unit(0.16, "in"), unit(1, "null")))
}

# ---- Combine parts into columns and auto-fit ----
build_panel <- function(ncol, base_text) {
  # legends for edges and nodes
  p_edges <- make_edge_plot(base_text)
  p_nodes <- make_node_plot(base_text)
  legs <- c(extract_all_guides(p_edges), extract_all_guides(p_nodes))
  if (length(legs) == 0) stop("No legends found.")

  # element grid (compact)
  el_grid <- make_element_grid(base_text, ncol = max(6, ceiling(length(stroke_map)/2)))

  # left-align each legend block
  leftify <- function(g) grobTree(g, vp = viewport(x = 0, just = c("left","top"), width = unit(1, "npc")))
  legs <- lapply(legs, leftify)
  el_grid <- leftify(el_grid)

  # assemble list and pad to grid
  blocks <- c(legs, list(el_grid))
  total <- length(blocks)
  rows <- ceiling(total / ncol)
  if (rows * ncol > total) {
    blocks <- c(blocks, replicate(rows * ncol - total, nullGrob(), simplify = FALSE))
  }
  grid_col <- gridExtra::arrangeGrob(grobs = blocks, nrow = rows, ncol = ncol,
                                     heights = unit(rep(1, rows), "null"),
                                     widths  = unit(rep(1, ncol), "null"))
  title_g <- textGrob("Legend", gp = gpar(fontface = "bold", cex = TITLE_CEX))
  arrangeGrob(grobs = list(title_g, grid_col), ncol = 1,
              heights = unit.c(unit(0.18, "in"), unit(1, "null")))
}

success <- FALSE
for (nc in CANDIDATE_COLS) {
  for (bt in CANDIDATE_TEXT) {
    panel <- build_panel(nc, bt)
    ggsave("legend_panel_CONSOLIDATED_UltraCompact.svg", panel, width = PANEL_WIDTH, height = PANEL_HEIGHT)
    ggsave("legend_panel_CONSOLIDATED_UltraCompact.png", panel, width = PANEL_WIDTH, height = PANEL_HEIGHT, dpi = 600)
    message(sprintf("Saved UltraCompact legend at %dx columns, base_text=%.1f", nc, bt))
    success <- TRUE
    break
  }
  if (success) break
}
if (!success) stop("Failed to save the ultra-compact legend.")

