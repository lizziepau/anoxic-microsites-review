
# ===========================================================
# Fitting-focused network plot (PATCHED): pagination + create_layout
# ===========================================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)
  library(igraph)
  library(ggrepel)
  library(ggforce)
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ---------------- Params ----------------
PAGINATE <- TRUE
PANELS_PER_PAGE <- 6
USE_WRAP <- TRUE
WRAP_NCOL <- 3
FIG_W <- 12
FIG_H <- 8.5
ELEMENTS_TO_SHOW <- unique(nodes$element)
MAX_LABEL_CHARS <- 22
LABEL_SIZE_BASE <- 2.6
BASE_TEXT_SIZE <- 10.5

# factors
edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$DOC <- factor(edges$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$doc_weight <- as.numeric(edges$doc_weight)

# filter elements
nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter(element %in% keep_el)
keep_names <- nodes$name
edges <- edges %>% filter(from %in% keep_names & to %in% keep_names)

# Manual lanes
lane_map <- c("solid"=0.05, "sorbed"=0.10, "aq/adsorbed"=0.15,
              "aq"=0.50, "aq/sorbed"=0.50,
              "solid/colloid"=0.90, "microsite"=0.98, "bulk"=0.02)
default_lane <- 0.5
nodes <- nodes %>% mutate(lane = ifelse(phase %in% names(lane_map), lane_map[phase], default_lane))
nodes <- nodes %>% mutate(lane = case_when(name=="Bulk oxic soil"~0.02,
                                           name=="Anoxic microsite"~0.98, TRUE~lane))

# y stacking
el_levels <- c("Fe","Mn","As","Cr","U","Se","Sb","Tc","Pu","S","V","default")
nodes$element <- factor(nodes$element, levels = el_levels)
y_map <- nodes %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes <- nodes %>% left_join(y_map, by="element")

coords <- nodes %>% transmute(name, x=lane, y=ypos)

# labels
wrap_label <- function(s, width=20) {
  s <- stringr::str_replace_all(s, " (ferrihydrite/goethite)", "")
  s <- stringr::str_wrap(s, width = width)
  s
}
nodes$label <- wrap_label(nodes$name, width = MAX_LABEL_CHARS)

# Sanitization
edges$mediator[is.na(edges$mediator) | trimws(edges$mediator)==""] <- "abiotic/biotic"
if (!"doc_weight" %in% names(edges)) edges$doc_weight <- 0.7

# graph + baked layout
g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
lay <- ggraph::create_layout(
  g, layout = "manual",
  x = coords$x[match(igraph::V(g)$name, coords$name)],
  y = coords$y[match(igraph::V(g)$name, coords$name)]
)

stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", S="#0099CC", Se="#33CC33", V="#AA4499",
  default="#666666"
)
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
lty_mediator <- c(
  "abiotic"=1, "abiotic/biotic"=2, "microbial"=3, "microbial/OM"=3, "Fe(II)/sulfide/OM"=2,
  "S²⁻"=2, "Mn(IV) oxides"=2, "O₂/Mn oxides"=2, "carbonate"=2, "microbial/Fe(II)/sulfide"=3,
  "O₂"=1, "Mn oxides/O₂"=2, "O₂/pH"=2, "Fe(II)/sulfide/bioreduction"=3, "Fe(II)/sulfide"=2,
  "microbial/Fe(II)"=3, "Mn/Fe oxides"=2, "O₂/NO₃⁻"=2, "carbonate/OM/O₂"=2
)
fill_map_phase <- c(
  "aq" = "#e6f5ff", "aq/sorbed" = "#e6f5ff",
  "solid" = "#f2e6ff", "solid/colloid" = "#f2e6ff",
  "sorbed" = "#fff2cc", "aq/adsorbed" = "#e6f5ff",
  "microsite" = "#ffe6e6", "bulk" = "#e6ffe6"
)

# base plot
base_plot <- ggraph::ggraph(lay) +
  ggraph::geom_edge_link(aes(color = mobility_effect,
                             linetype = mediator,
                             width = doc_weight),
                         alpha = 0.5,
                         arrow = grid::arrow(length = unit(2, "mm")),
                         end_cap = ggraph::circle(1.8, 'mm'),
                         show.legend = TRUE) +
  scale_edge_width(range = c(0.3, 1.1), guide = "none") +
  scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
  scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
  ggraph::geom_node_point(aes(shape = class, fill = phase, color = element),
                          size = 2.8, stroke = 0.9) +
  scale_shape_manual(values = c(species=21, sorbent=22, ligand=23, electron_acceptor=24, environment=25)) +
  scale_fill_manual(values = fill_map_phase) +
  scale_color_manual(values = stroke_map, guide = "none") +
  ggraph::geom_node_text(aes(label = label),
                         size = LABEL_SIZE_BASE, vjust = -0.9, lineheight = 0.98) +
  coord_cartesian(xlim = c(0,1), expand = TRUE) +
  theme_minimal(base_size = BASE_TEXT_SIZE) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.key.width = unit(10, "pt"),
    legend.key.height = unit(10, "pt"),
    panel.spacing.x = unit(1.5, "mm"),
    panel.spacing.y = unit(1.5, "mm")
  ) +
  labs(
    title = "Reaction network in oxic soil with anoxic microsites",
    subtitle = "Faceted by DOC and pH (edge width scales with DOC; color = mobilization vs immobilization)",
    x = NULL, y = NULL
  )

# ---- Faceting & export ----
if (PAGINATE) {
  edges$facet_key <- interaction(edges$DOC, edges$pH_band, edges$context, sep = " • ")
  plot1 <- base_plot + ggforce::facet_wrap_paginate(~ facet_key, ncol = WRAP_NCOL,
                                                    nrow = ceiling(PANELS_PER_PAGE / WRAP_NCOL), page = 1)
  pages <- ggforce::n_pages(plot1)
  for (p in seq_len(pages)) {
    pplot <- base_plot + ggforce::facet_wrap_paginate(~ facet_key, ncol = WRAP_NCOL,
                                                      nrow = ceiling(PANELS_PER_PAGE / WRAP_NCOL), page = p)
    ggsave(sprintf("reaction_network_fitting_page%02d.svg", p), pplot, width = FIG_W, height = FIG_H)
    ggsave(sprintf("reaction_network_fitting_page%02d.png", p), pplot, width = FIG_W, height = FIG_H, dpi = 600)
  }
} else {
  if (USE_WRAP) {
    base_plot <- base_plot + facet_wrap(~ DOC + pH_band + context, ncol = WRAP_NCOL)
  } else {
    base_plot <- base_plot + facet_grid(DOC + context ~ pH_band)
  }
  ggsave("reaction_network_fitting.svg", base_plot, width = FIG_W, height = FIG_H)
  ggsave("reaction_network_fitting.png", base_plot, width = FIG_W, height = FIG_H, dpi = 600)
}
