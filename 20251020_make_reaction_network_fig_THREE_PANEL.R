
# ===================================================================
# Three-panel plate (7.0" x 4.167")
# Based on: reaction_network_redox_DOC_legible_FORMULA_LEGEND.R logic
# Panels chosen (edit FACETS_TO_INCLUDE below):
#   1) Moderate DOC • circumneutral • Bulk oxic soil
#   2) Moderate DOC • circumneutral • Anoxic microsite
#   3) High DOC     • circumneutral • Anoxic microsite
# ===================================================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)
  library(igraph)
  library(patchwork)
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ------- Which three facet panels to include (edit as needed) -------
FACETS_TO_INCLUDE <- c(
  "Moderate DOC (2–10 mg C/L) • circumneutral (6.0–7.5) • Bulk oxic soil",
  "Moderate DOC (2–10 mg C/L) • circumneutral (6.0–7.5) • Anoxic microsite",
  "High DOC (>10 mg C/L) • circumneutral (6.0–7.5) • Anoxic microsite"
)

# Figure size and columns
FIG_W <- 7
FIG_H <- 4.167
NCOL <- 3

# Focus elements for labeling
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V")
LABEL_SORBENTS <- FALSE
LABEL_LIGANDS  <- FALSE
LABEL_SIZE <- 2.1
BASE_TEXT <- 8.6

# ================== factor setup ==================
edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$DOC <- factor(edges$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$doc_weight <- as.numeric(edges$doc_weight)

# ================== node filtering ==================
nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter(element %in% keep_el)
edges <- edges %>% filter(from %in% nodes$name & to %in% nodes$name)

# ================== manual lanes ==================
lane_map <- c("solid"=0.05, "sorbed"=0.10, "aq/adsorbed"=0.15,
              "aq"=0.50, "aq/sorbed"=0.50,
              "solid/colloid"=0.90, "microsite"=0.98, "bulk"=0.02)
default_lane <- 0.5
nodes <- nodes %>% mutate(lane = ifelse(phase %in% names(lane_map), lane_map[phase], default_lane))
nodes <- nodes %>% mutate(lane = case_when(name=="Bulk oxic soil"~0.02,
                                           name=="Anoxic microsite"~0.98, TRUE~lane))

# y stacking by element
el_levels <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V","default")
nodes$element <- factor(nodes$element, levels = el_levels)
y_map <- nodes %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes <- nodes %>% left_join(y_map, by="element")
coords <- nodes %>% transmute(name, x=lane, y=ypos)

# ============== formula-only labels =================
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
    "Se\\(VI\\)/\\(IV\\) oxyanions \\(aq/sorbed\\)" = "Se(VI/IV)_aq/sorb",
    "Se\\(0\\) \\(s\\)" = "Se(0)(s)",
    "FeSe \\(s\\)" = "FeSe(s)",
    "V\\(V\\) \\(vanadate\\) \\(aq/sorbed\\)" = "V(V)_aq/sorb",
    "V\\(IV/III\\) \\(adsorbed/oxide\\)" = "V(IV/III)_ox/ads",
    "Organic ligands \\(OM\\)" = "OM",
    "Sulfide \\(S²⁻/HS⁻\\)" = "S2-/HS-",
    "O₂ \\(bulk soil\\)" = "O2",
    "Bulk oxic soil" = "Bulk (oxic)",
    "Anoxic microsite" = "Microsite (anoxic)"
  )
  for (pat in names(repl)) s <- gsub(pat, repl[[pat]], s, perl = TRUE)
  s <- gsub("\\s+", " ", s); trimws(s)
}
nodes$label <- vapply(nodes$name, shorten_label, character(1))

# ============== label thinning ==================
pick_label_nodes <- function(layout_df, focus_elements, label_sorbents = FALSE, label_ligands = FALSE) {
  d <- layout_df
  is_focus <- d$element %in% focus_elements
  keep <- (d$class == "species" & is_focus) | (d$class == "environment")
  if (label_sorbents) keep <- keep | (d$class == "sorbent")
  if (label_ligands) keep <- keep | (d$class == "ligand")
  keep
}

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
fill_map_phase <- c("aq"="#e6f5ff","aq/sorbed"="#e6f5ff","solid"="#f2e6ff","solid/colloid"="#f2e6ff",
                    "sorbed"="#fff2cc","aq/adsorbed"="#e6f5ff","microsite"="#ffe6e6","bulk"="#e6ffe6")

# ---------- draw a single panel ----------
draw_panel <- function(sub_edges, panel_title) {
  g <- igraph::graph_from_data_frame(d = sub_edges, vertices = nodes, directed = TRUE)
  lay <- ggraph::create_layout(g, layout = "manual",
                               x = coords$x[match(igraph::V(g)$name, coords$name)],
                               y = coords$y[match(igraph::V(g)$name, coords$name)])
  layout_df <- as.data.frame(lay)
  label_mask <- pick_label_nodes(layout_df, focus_elements = ELEMENTS_TO_SHOW,
                                 label_sorbents = LABEL_SORBENTS, label_ligands = LABEL_LIGANDS)

  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator, width = doc_weight),
                           alpha = 0.55,
                           arrow = grid::arrow(length = unit(1.6, "mm")),
                           end_cap = ggraph::circle(1.6, 'mm'),
                           show.legend = TRUE) +
    scale_edge_width(range = c(0.35, 0.9), guide = "none") +
    scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
    scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
    ggraph::geom_node_point(aes(shape = class, fill = phase, color = element),
                            size = 2.4, stroke = 0.9, show.legend = TRUE) +
    scale_shape_manual(values = c(species=21, sorbent=22, ligand=23, electron_acceptor=24, environment=25),
                       name = "Node class") +
    scale_fill_manual(values = fill_map_phase, name = "Phase") +
    scale_color_manual(values = stroke_map, name = "Element") +
    ggraph::geom_node_text(aes(label = ifelse(label_mask, label, "")),
                           size = LABEL_SIZE, vjust = -0.9, lineheight = 0.98, check_overlap = TRUE) +
    coord_cartesian(xlim = c(0,1), expand = TRUE) +
    theme_minimal(base_size = BASE_TEXT) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = BASE_TEXT + 0.5),
      plot.margin = margin(1,1,1,1),
      legend.position = "right",
      legend.key.width = unit(7, "pt"),
      legend.key.height = unit(7, "pt"),
      legend.text = element_text(size = BASE_TEXT - 1),
      legend.title = element_text(size = BASE_TEXT)
    ) +
    labs(title = panel_title, x = NULL, y = NULL)
}

# Build subset
edges <- edges %>% mutate(facet_key = interaction(DOC, pH_band, context, sep = " • "))
edges_sub <- edges %>% filter(facet_key %in% FACETS_TO_INCLUDE)

plots <- lapply(FACETS_TO_INCLUDE, function(fk) {
  sub_edges <- edges_sub %>% filter(facet_key == fk)
  draw_panel(sub_edges, panel_title = as.character(fk))
})

plate <- wrap_plots(plots, ncol = NCOL, byrow = TRUE, guides = "collect") +
  plot_annotation(
    title = "Reaction network in oxic soil with anoxic microsites",
    subtitle = "Three-panel summary at circumneutral pH • Formula-only labels • Focus elements labeled",
    theme = theme(
      plot.title = element_text(face = "bold", size = BASE_TEXT + 2),
      plot.subtitle = element_text(size = BASE_TEXT + 0.5),
      legend.position = "right"
    )
  ) & theme(legend.position = "right")

ggsave("reaction_network_THREE_PANEL.svg", plate, width = FIG_W, height = FIG_H)
ggsave("reaction_network_THREE_PANEL.png", plate, width = FIG_W, height = FIG_H, dpi = 600)
