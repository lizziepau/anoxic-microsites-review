
# ===================================================================
# Legible reaction network (FORMULA LABELS): panel-by-panel with patchwork
# ===================================================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)
  library(igraph)
  library(patchwork)
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

NCOL <- 3
FIG_W <- 16
FIG_H <- 12
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Se")  # set to unique(nodes$element) to show all
FACETS_TO_INCLUDE <- NULL
LABEL_SIZE <- 3.0
BASE_TEXT <- 11

edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$DOC <- factor(edges$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$doc_weight <- as.numeric(edges$doc_weight)

nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter(element %in% keep_el)
keep_names <- nodes$name
edges <- edges %>% filter(from %in% keep_names & to %in% keep_names)

lane_map <- c("solid"=0.05, "sorbed"=0.10, "aq/adsorbed"=0.15,
              "aq"=0.50, "aq/sorbed"=0.50,
              "solid/colloid"=0.90, "microsite"=0.98, "bulk"=0.02)
default_lane <- 0.5
nodes <- nodes %>% mutate(lane = ifelse(phase %in% names(lane_map), lane_map[phase], default_lane))
nodes <- nodes %>% mutate(lane = case_when(name=="Bulk oxic soil"~0.02,
                                           name=="Anoxic microsite"~0.98, TRUE~lane))

el_levels <- c("Fe","Mn","As","Cr","U","Se","Sb","Tc","Pu","S","V","default")
nodes$element <- factor(nodes$element, levels = el_levels)
y_map <- nodes %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes <- nodes %>% left_join(y_map, by="element")

coords <- nodes %>% transmute(name, x=lane, y=ypos)


shorten_label <- function(s) {
  s <- trimws(s)

  repl <- list(
    "Fe\\(III\\) \\(ferrihydrite/goethite\\)" = "Fe(III)",
    "Fe\\(II\\) \\(aq\\)" = "Fe(II)_aq",
    "FeS \\(mackinawite/pyrite\\)" = "FeS",
    "Siderite \\(FeCO₃\\)" = "FeCO3(s)",
    "Fe\\(III\\) oxides" = "Fe(III)Ox(s)",

    "Mn\\(IV\\) oxides \\(birnessite\\)" = "Mn(IV)Ox(s)",
    "Mn\\(IV\\) oxides" = "Mn(IV)Ox(s)",
    "Mn\\(II\\) \\(aq\\)" = "Mn(II)_aq",

    "As\\(V\\) \\(arsenate\\) sorbed" = "As(V)_sorbed",
    "As\\(III\\) \\(arsenite\\) \\(aq\\)" = "As(III)_aq",
    "As₂S₃ \\(orpiment\\)" = "As2S3(s)",

    "Sb\\(V\\) \\(antimonate\\) sorbed" = "Sb(V)_sorbed",
    "Sb\\(III\\) \\(aq\\)" = "Sb(III)_aq",
    "Sb₂S₃ \\(stibnite\\)" = "Sb2S3(s)",

    "Cr\\(VI\\) \\(chromate\\) \\(aq\\)" = "CrO4^2-(aq)",
    "Cr\\(III\\) \\(hydroxide/oxyhydroxide\\) \\(s\\)" = "Cr(III)OH(s)",

    "Tc\\(VII\\) \\(TcO₄⁻\\) \\(aq\\)" = "TcO4^-(aq)",
    "Tc\\(IV\\) \\(TcO₂·nH₂O\\) \\(s\\)" = "TcO2·nH2O(s)",

    "Pu\\(V/VI\\) \\(aq/complexed\\)" = "Pu(V/VI)_aq",
    "Pu\\(IV\\) hydroxo/oxide \\(s/colloid\\)" = "Pu(IV)_ox(s/col)",

    "U\\(VI\\)–carbonate complexes \\(aq\\)" = "U(VI)-CO3(aq)",
    "U\\(IV\\) \\(UO₂\\) \\(s\\)" = "UO2(s)",

    "Sulfate \\(SO₄²⁻\\)" = "SO4^2-",
    "HS⁻/H₂S" = "HS-/H2S",

    "Se\\(VI\\)/\\(IV\\) oxyanions \\(aq/sorbed\\)" = "Se(VI/IV)_aq/sorbed",
    "Se\\(0\\) \\(s\\)" = "Se(0)(s)",
    "FeSe \\(s\\)" = "FeSe(s)",

    "V\\(V\\) \\(vanadate\\) \\(aq/sorbed\\)" = "V(V)_aq/sorbed",
    "V\\(IV/III\\) \\(adsorbed/oxide\\)" = "V(IV/III)_ox/ads",

    "Organic ligands \\(OM\\)" = "OM",
    "Sulfide \\(S²⁻/HS⁻\\)" = "S2-/HS-",
    "O₂ \\(bulk soil\\)" = "O2",
    "Bulk oxic soil" = "Bulk (oxic)",
    "Anoxic microsite" = "Microsite (anoxic)"
  )

  for (pat in names(repl)) {
    s <- gsub(pat, repl[[pat]], s, perl = TRUE)
  }

  # generic cleanups
  s <- gsub("\\s+", " ", s)
  s <- trimws(s)
  s
}


nodes$label <- vapply(nodes$name, shorten_label, character(1))

edges$mediator[is.na(edges$mediator) | trimws(edges$mediator)==""] <- "abiotic/biotic"
if (!"doc_weight" %in% names(edges)) edges$doc_weight <- 0.7

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
  "solid" = "#f2e6ff", "solid/colloid" = "#f2e6f5",
  "sorbed" = "#fff2cc", "aq/adsorbed" = "#e6f5ff",
  "microsite" = "#ffe6e6", "bulk" = "#e6ffe6"
)

draw_panel <- function(sub_edges, panel_title) {
  g <- igraph::graph_from_data_frame(d = sub_edges, vertices = nodes, directed = TRUE)
  lay <- ggraph::create_layout(
    g, layout = "manual",
    x = coords$x[match(igraph::V(g)$name, coords$name)],
    y = coords$y[match(igraph::V(g)$name, coords$name)]
  )
  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator, width = doc_weight),
                           alpha = 0.6,
                           arrow = grid::arrow(length = unit(2.2, "mm")),
                           end_cap = ggraph::circle(2.0, 'mm'),
                           show.legend = FALSE) +
    scale_edge_width(range = c(0.4, 1.2), guide = "none") +
    scale_edge_color_manual(values = col_mob) +
    scale_edge_linetype_manual(values = lty_mediator) +
    ggraph::geom_node_point(aes(shape = class, fill = phase, color = element),
                            size = 3.2, stroke = 1, show.legend = FALSE) +
    scale_shape_manual(values = c(species=21, sorbent=22, ligand=23, electron_acceptor=24, environment=25)) +
    scale_fill_manual(values = fill_map_phase) +
    scale_color_manual(values = stroke_map, guide = "none") +
    ggraph::geom_node_text(aes(label = label), size = LABEL_SIZE, vjust = -0.9, lineheight = 0.98) +
    coord_cartesian(xlim = c(0,1), expand = TRUE) +
    theme_minimal(base_size = BASE_TEXT) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = BASE_TEXT + 1),
      plot.margin = margin(2,2,2,2)
    ) +
    labs(title = panel_title, x = NULL, y = NULL)
}

edges <- edges %>% mutate(facet_key = interaction(DOC, pH_band, context, sep = " • "))
if (!is.null(FACETS_TO_INCLUDE)) {
  edges <- edges %>% filter(facet_key %in% FACETS_TO_INCLUDE)
}
facet_levels <- unique(edges$facet_key)

plots <- vector("list", length(facet_levels))
for (i in seq_along(facet_levels)) {
  fk <- facet_levels[i]
  sub_edges <- edges %>% filter(facet_key == fk)
  plots[[i]] <- draw_panel(sub_edges, panel_title = as.character(fk))
}

plate <- wrap_plots(plots, ncol = NCOL, byrow = TRUE) +
  plot_annotation(
    title = "Reaction network in oxic soil with anoxic microsites",
    subtitle = "Formula-only labels • Panels = DOC × pH × context • Edge width scales with DOC",
    theme = theme(
      plot.title = element_text(face = "bold", size = BASE_TEXT + 3),
      plot.subtitle = element_text(size = BASE_TEXT + 1)
    )
  )

ggsave("reaction_network_redox_DOC_legible_FORMULA.svg", plate, width = FIG_W, height = FIG_H)
ggsave("reaction_network_redox_DOC_legible_FORMULA.png", plate, width = FIG_W, height = FIG_H, dpi = 600)
