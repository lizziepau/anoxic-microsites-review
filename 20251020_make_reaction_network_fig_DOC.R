
# =========================
# Reaction network with DOC levels: Oxic soil + anoxic microsites (pH 4.5–8.5)
# =========================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)      # install.packages("ggraph")
  library(igraph)      # install.packages("igraph")
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$DOC <- factor(edges$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$doc_weight <- as.numeric(edges$doc_weight)

shape_map <- c(
  species = 21, sorbent = 22, ligand = 23, electron_acceptor = 24, environment = 25
)
fill_map_phase <- c(
  "aq" = "#e6f5ff", "aq/sorbed" = "#e6f5ff", "solid" = "#f2e6ff",
  "solid/colloid" = "#f2e6ff", "sorbed" = "#fff2cc", "aq/adsorbed" = "#e6f5ff",
  "microsite" = "#ffe6e6", "bulk" = "#e6ffe6"
)
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", S="#0099CC", Se="#33CC33", V="#AA4499",
  default="#666666"
)
nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")

lty_mediator <- c(
  "abiotic"=1, "abiotic/biotic"=2, "microbial"=3, "microbial/OM"=3, "Fe(II)/sulfide/OM"=2,
  "S²⁻"=2, "Mn(IV) oxides"=2, "O₂/Mn oxides"=2, "carbonate"=2, "microbial/Fe(II)/sulfide"=3,
  "O₂"=1, "Mn oxides/O₂"=2, "O₂/pH"=2, "Fe(II)/sulfide/bioreduction"=3, "Fe(II)/sulfide"=2,
  "microbial/Fe(II)"=3, "Mn/Fe oxides"=2, "O₂/NO₃⁻"=2, "carbonate/OM/O₂"=2
)

edges$mediator[is.na(edges$mediator) | edges$mediator==""] <- "abiotic/biotic"
edges$lty <- ifelse(edges$mediator %in% names(lty_mediator), edges$mediator, "abiotic/biotic")

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

p <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(color = mobility_effect, linetype = lty, width = doc_weight),
                 arrow = arrow(length = unit(2.5, "mm")),
                 end_cap = circle(2.2, 'mm'),
                 show.legend = TRUE) +
  scale_edge_width(range = c(0.4, 1.1), guide = "none") +
  geom_node_point(aes(shape = class, fill = phase, stroke = 1.2, color = element), size = 3) +
  scale_shape_manual(values = shape_map) +
  scale_fill_manual(values = fill_map_phase, breaks = names(fill_map_phase)) +
  scale_color_manual(values = stroke_map, guide = "none") +
  geom_node_text(aes(label = name), size = 2.6, vjust = -1.2, lineheight = 0.98) +
  scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
  scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
  facet_grid(context + DOC ~ pH_band) +
  theme_minimal(base_size = 10.5) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  ) +
  labs(
    title = "Reaction network for metals in oxic soils with anoxic microsites",
    subtitle = "pH 4.5–8.5 • Rows = context × DOC • Columns = pH bands • Edge color = mobilization vs immobilization • Edge width scales with DOC",
    x = NULL, y = NULL
  )

ggsave("reaction_network_redox_DOC.svg", p, width = 13.5, height = 12.0)
ggsave("reaction_network_redox_DOC.png", p, width = 13.5, height = 12.0, dpi = 600)
