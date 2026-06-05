
# ===================================================================
# COMPLETE LEGEND PANEL (standalone figure)
# Exports: legend_panel_COMPLETE.svg / .png
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# ---- Aesthetics (match the main figures) ----
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

# DOC levels shown as sample linewidths for clarity in the legend
DOC_levels <- c("Low DOC", "Moderate DOC", "High DOC")
DOC_linewidths <- c("Low DOC" = 0.4, "Moderate DOC" = 0.8, "High DOC" = 1.2)

# ---- Build a dummy canvas that carries ALL scales into one legend ----

# 1) Edge legend (color, linetype)
edge_df <- expand.grid(
  mobility_effect = names(col_mob),
  mediator_class  = names(proc_types)
) |>
  mutate(x = 0, xend = 1,
         y = row_number(), yend = y,
         DOC_level = factor(rep(DOC_levels, length.out = n()), levels = DOC_levels))

# 2) Node legend (shape, fill, color)
node_df <- expand.grid(
  mobility_state = names(shape_map),
  phase_norm     = names(fill_map_phase),
  element        = names(stroke_map)
) |>
  mutate(x = 0.5,
         y = row_number() + max(edge_df$y) + 1)

# 3) DOC sample-only legend rows
doc_df <- tibble(
  DOC_level = factor(DOC_levels, levels = DOC_levels),
  x = 0, xend = 1,
  y = seq_len(length(DOC_levels)) + max(node_df$y) + 1,
  yend = y
)

# ---- Compose single plot that only renders legends ----
p <- ggplot() +
  # Edges (effect on mobility + process type + sample linewidth mapped to DOC)
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

  # Nodes (mobility state + phase fill + element stroke color)
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

  # DOC sample-only lines (if you want explicit labeled swatches below the node legends)
  geom_segment(
    data = doc_df,
    aes(x = x, y = y, xend = xend, yend = yend, linewidth = DOC_level),
    inherit.aes = FALSE, show.legend = FALSE
  ) +

  # Canvas/Theme
  theme_void(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text  = element_text(size = 8),
    plot.margin  = margin(4, 8, 4, 8)
  )

# Extract only the combined legend
leg <- cowplot::get_legend(p)

# Save legend panel
ggsave("legend_panel_COMPLETE.svg", leg, width = 7, height = 3.0)
ggsave("legend_panel_COMPLETE.png", leg, width = 7, height = 3.0, dpi = 600)
