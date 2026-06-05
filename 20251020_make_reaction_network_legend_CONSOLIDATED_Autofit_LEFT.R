
# ===================================================================
# CONSOLIDATED LEGEND — AUTO-FIT, LEFT-ALIGNED CONTENT
# - Single page/panel legend arranged in columns
# - Omits DOC level and Phase
# - Left-aligns each legend block so contents don't drift right
# - Tries multiple column counts & font sizes to fit the panel
# Outputs: legend_panel_CONSOLIDATED_Autofit_LEFT.svg/.png
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(gridExtra)
  library(grid)
})

# -------- Target panel size (inches) --------
PANEL_WIDTH  <- 7.0
PANEL_HEIGHT <- 1.6

# -------- Candidate layouts (columns × base text size) --------
CANDIDATE_COLS <- c(4, 3, 2)
CANDIDATE_TEXT <- c(8.6, 8.2, 7.8, 7.2)

TITLE_CEX <- 1.05

# ---- Aesthetics (match main figures) ----
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666"
)
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
proc_types <- c(solid = 1, dashed = 2, dotted = 3)
shape_map <- c("Mobile (aq)" = 21, "Immobile/Sorbed" = 22, "Other" = 25)

# ---- Build small helper plots to harvest guide boxes ----
make_edge_plot <- function(base_text) {
  edge_df <- expand.grid(
    mobility_effect = names(col_mob),
    mediator_class  = names(proc_types)
  ) |>
    mutate(x = 0, xend = 1, y = row_number(), yend = y)

  ggplot(edge_df) +
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
    theme_void(base_size = base_text) +
    theme(legend.position = "bottom")
}

make_node_plot <- function(base_text) {
  node_df <- expand.grid(
    mobility_state = names(shape_map),
    element        = names(stroke_map)
  ) |>
    mutate(x = 0.5, y = row_number())

  ggplot(node_df) +
    geom_point(aes(x = x, y = y, shape = mobility_state, color = element),
               size = 3.0, stroke = 0.9, show.legend = TRUE) +
    scale_shape_manual(values = shape_map, name = "Mobility state") +
    scale_colour_manual(values = stroke_map, name = "Element") +
    guides(
      shape = guide_legend(order = 1, override.aes = list(color = "grey20")),
      colour= guide_legend(order = 2)
    ) +
    theme_void(base_size = base_text) +
    theme(legend.position = "bottom")
}

extract_all_guides <- function(p) cowplot::get_plot_component(p, "guide-box", return_all = TRUE)

# Wrap each legend grob to force LEFT alignment inside its cell
wrap_left <- function(g) grobTree(g, vp = viewport(x = 0, just = c("left","top"), width = unit(1, "npc")))

build_panel <- function(ncol, base_text) {
  p_edges <- make_edge_plot(base_text)
  p_nodes <- make_node_plot(base_text)
  legs <- c(extract_all_guides(p_edges), extract_all_guides(p_nodes))
  if (length(legs) == 0) stop("No legends found to arrange.")
  legs <- lapply(legs, wrap_left)  # force left alignment
  rows <- ceiling(length(legs) / ncol)
  # pad to full grid at the END so empties land on the right
  if (rows * ncol > length(legs)) {
    legs <- c(legs, replicate(rows * ncol - length(legs), grid::nullGrob(), simplify = FALSE))
  }
  grid_col <- gridExtra::arrangeGrob(grobs = legs, nrow = rows, ncol = ncol,
                                     heights = unit(rep(1, rows), "null"),
                                     widths  = unit(rep(1, ncol), "null"))
  title_g <- textGrob("Legend", gp = gpar(fontface = "bold", cex = TITLE_CEX))
  arrangeGrob(grobs = list(title_g, grid_col), ncol = 1,
              heights = unit.c(unit(0.18, "in"), unit(1, "null")))
}

# Try candidates and save first success
success <- FALSE
for (nc in CANDIDATE_COLS) {
  for (bt in CANDIDATE_TEXT) {
    panel <- build_panel(nc, bt)
    fn_svg <- "legend_panel_CONSOLIDATED_Autofit_LEFT.svg"
    fn_png <- "legend_panel_CONSOLIDATED_Autofit_LEFT.png"
    ggsave(fn_svg, panel, width = PANEL_WIDTH, height = PANEL_HEIGHT)
    ggsave(fn_png, panel, width = PANEL_WIDTH, height = PANEL_HEIGHT, dpi = 600)
    message(sprintf("Saved LEFT-aligned legend with %d columns and base_text = %.1f", nc, bt))
    success <- TRUE
    break
  }
  if (success) break
}
if (!success) stop("Unable to save left-aligned legend panel.")

