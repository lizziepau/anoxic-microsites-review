
# ===============================================
# Legible reaction network (PATCHED): manual layout baked via create_layout
# ===============================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)
  library(igraph)
  library(ggrepel)
})

# ------------------- Inputs -------------------
nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ---- tuning knobs ----
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Se")  # reduce initially; set to unique(nodes$element) to show all
FACET_DOC <- TRUE        # if FALSE, collapses DOC into one row (uses median weight)
EDGE_ALPHA <- 0.6
EDGE_WIDTH_RANGE <- c(0.4, 1.4)
LABEL_WRAP <- 26         # characters

# ------------------- Preprocess -------------------
# Default color map (fix for empty element values via "default")
stroke_map <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", S="#0099CC", Se="#33CC33", V="#AA4499",
  default="#666666"
)
nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"

# keep only requested elements plus neutral infrastructure nodes
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes_keep <- nodes %>% filter(element %in% keep_el)
edges_keep <- edges %>%
  filter((from %in% nodes_keep$name) & (to %in% nodes_keep$name))

# Factor facets
edges_keep$pH_band <- factor(edges_keep$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges_keep$context <- factor(edges_keep$context, levels = c("Bulk oxic soil","Anoxic microsite"))
stopifnot("DOC" %in% names(edges_keep))
edges_keep$DOC <- factor(edges_keep$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges_keep$doc_weight <- as.numeric(edges_keep$doc_weight)

# Collapse DOC if needed
if (!FACET_DOC) {
  edges_keep <- edges_keep %>%
    group_by(from, to, process, context, mobility_effect, mediator, pH_band) %>%
    summarize(DOC = "All DOC", doc_weight = median(doc_weight, na.rm=TRUE), .groups="drop")
  edges_keep$DOC <- factor(edges_keep$DOC, levels = "All DOC")
}

# Sanitize mediator + doc_weight
edges_keep$mediator[is.na(edges_keep$mediator) | trimws(edges_keep$mediator)==""] <- "abiotic/biotic"
if (!"doc_weight" %in% names(edges_keep)) edges_keep$doc_weight <- 0.7

# ------------------- Manual layout -------------------
lane_map <- c(
  "solid" = 0.05, "sorbed" = 0.10, "aq/adsorbed" = 0.15,
  "aq" = 0.50, "aq/sorbed" = 0.50,
  "solid/colloid" = 0.90, "microsite" = 0.98, "bulk" = 0.02
)
default_lane <- 0.5

# ensure environment anchors
nodes_keep <- nodes_keep %>%
  mutate(lane = ifelse(phase %in% names(lane_map), lane_map[phase], default_lane),
         lane = case_when(
           name == "Bulk oxic soil" ~ 0.02,
           name == "Anoxic microsite" ~ 0.98,
           TRUE ~ lane
         ))

# y coordinate by element group
el_levels <- c("Fe","Mn","As","Cr","U","Se","Sb","Tc","Pu","S","V","default")
nodes_keep$element <- factor(nodes_keep$element, levels = el_levels)
y_map <- nodes_keep %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes_keep <- nodes_keep %>% left_join(y_map, by="element")

coords <- nodes_keep %>% transmute(name, x = lane, y = ypos)

# Wrap labels
wrap_lab <- function(x, width) stringr::str_replace_all(stringr::str_wrap(x, width = width), "\n", "\n")
nodes_keep$label <- wrap_lab(nodes_keep$name, LABEL_WRAP)

# ------------------- Build graph & baked layout -------------------
g <- graph_from_data_frame(d = edges_keep, vertices = nodes_keep, directed = TRUE)

lay <- ggraph::create_layout(
  g, layout = "manual",
  x = coords$x[match(igraph::V(g)$name, coords$name)],
  y = coords$y[match(igraph::V(g)$name, coords$name)]
)

# ------------------- Aesthetics -------------------
col_mob <- c(mobilize = "#3b5b92", immobilize = "#a35e10", neutral = "#7a7a7a")
lty_mediator <- c(
  "abiotic"=1, "abiotic/biotic"=2, "microbial"=3, "microbial/OM"=3, "Fe(II)/sulfide/OM"=2,
  "S²⁻"=2, "Mn(IV) oxides"=2, "O₂/Mn oxides"=2, "carbonate"=2, "microbial/Fe(II)/sulfide"=3,
  "O₂"=1, "Mn oxides/O₂"=2, "O₂/pH"=2, "Fe(II)/sulfide/bioreduction"=3, "Fe(II)/sulfide"=2,
  "microbial/Fe(II)"=3, "Mn/Fe oxides"=2, "O₂/NO₃⁻"=2, "carbonate/OM/O₂"=2
)
fill_map_phase <- c(
  "aq"="#e6f5ff","aq/sorbed"="#e6f5ff","aq/adsorbed"="#e6f5ff",
  "solid"="#f2e6ff","solid/colloid"="#f2e6ff","sorbed"="#fff2cc",
  "microsite"="#ffe6e6","bulk"="#e6ffe6"
)

base_text <- 11
label_size <- 3

p <- ggraph::ggraph(lay) +
  ggraph::geom_edge_link(
    aes(color = mobility_effect, linetype = mediator, width = doc_weight),
    alpha = EDGE_ALPHA,
    arrow = grid::arrow(length = unit(2.2, "mm")),
    end_cap = ggraph::circle(2.0, 'mm'),
    show.legend = TRUE
  ) +
  scale_edge_width(range = EDGE_WIDTH_RANGE, guide = "none") +
  scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
  scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
  ggraph::geom_node_point(aes(shape = class, fill = phase, color = element), size = 3.2, stroke = 1) +
  scale_shape_manual(values = c(species=21, sorbent=22, ligand=23, electron_acceptor=24, environment=25)) +
  scale_fill_manual(values = fill_map_phase) +
  scale_color_manual(values = stroke_map, guide = "none") +
  ggraph::geom_node_text(aes(label = label), size = label_size, vjust = -0.9, lineheight = 0.98) +
  coord_cartesian(xlim = c(0,1), expand = TRUE) +
  facet_grid(context + DOC ~ pH_band, scales = "free_y") +
  theme_minimal(base_size = base_text) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  ) +
  labs(
    title = "Legible reaction network: oxic soil with anoxic microsites (pH 4.5–8.5)",
    subtitle = "Manual phase lanes • Edge color = mobilization vs immobilization • Edge width scales with DOC",
    x = NULL, y = NULL
  )

ggsave("reaction_network_redox_DOC_legible.svg", p, width = 16, height = 12)
ggsave("reaction_network_redox_DOC_legible.png", p, width = 16, height = 12, dpi = 600)
