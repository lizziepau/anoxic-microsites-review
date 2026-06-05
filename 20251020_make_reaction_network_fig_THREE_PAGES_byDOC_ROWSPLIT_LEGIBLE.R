
# ===================================================================
# THREE DOC LEVELS -> SIX PAGES (per-context): 1×3 pH panels
# Legibility upgrades:
#   • Label thinning: at most MAX_LABELS_PER_ELEMENT labels per element (phase-prioritized)
#   • Small x-nudge for labels by element to avoid vertical stacking
#   • Legend exported separately ("legend_only_byDOC.png/svg")
# Canvas: 7.0" × 4.167" (journal)
# ===================================================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph); library(igraph); library(patchwork); library(ggrepel); library(cowplot)
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7
FIG_H <- 4.167
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V")
MAX_LABELS_PER_ELEMENT <- 2
LABEL_SORBENTS <- FALSE
LABEL_LIGANDS  <- FALSE
LABEL_SIZE <- 1.9
BASE_TEXT <- 8.0

edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$DOC     <- factor(edges$DOC,     levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- as.numeric(edges$doc_weight)

nodes$element[is.na(nodes$element) | nodes$element==""] <- "default"
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter(element %in% keep_el)
edges <- edges %>% filter(from %in% nodes$name & to %in% nodes$name)

lane_map <- c("solid"=0.05, "sorbed"=0.10, "aq/adsorbed"=0.15,
              "aq"=0.50, "aq/sorbed"=0.50,
              "solid/colloid"=0.90, "microsite"=0.98, "bulk"=0.02)
default_lane <- 0.5
nodes <- nodes %>% mutate(lane = ifelse(phase %in% names(lane_map), lane_map[phase], default_lane))
nodes <- nodes %>% mutate(lane = case_when(name=="Bulk oxic soil"~0.02,
                                           name=="Anoxic microsite"~0.98, TRUE~lane))

el_levels <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V","default")
nodes$element <- factor(nodes$element, levels = el_levels)
y_map <- nodes %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes <- nodes %>% left_join(y_map, by="element")
coords <- nodes %>% transmute(name, x=lane, y=ypos)

nodes <- nodes %>% mutate(
  mobility_state = case_when(
    phase %in% c("aq","aq/sorbed","aq/adsorbed") ~ "Mobile (aq)",
    phase %in% c("sorbed","solid","solid/colloid") ~ "Immobile/Sorbed",
    TRUE ~ "Other"
  )
)

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
    "Sulfide \\(S²⁻/HS⁻\\)" = "S2-/HS-"
  )
  for (pat in names(repl)) s <- gsub(pat, repl[[pat]], s, perl = TRUE)
  s <- gsub("\\s+", " ", s); trimws(s)
}
nodes$label <- vapply(nodes$name, shorten_label, character(1))

# rank phases for label selection
phase_rank <- c("aq"=1, "aq/adsorbed"=2, "aq/sorbed"=3, "sorbed"=4, "solid"=5, "solid/colloid"=6, "bulk"=7, "microsite"=7)
# degree proxy for centrality
# degree proxy for centrality
deg_tbl <- edges %>%
  count(from, name = "out_deg") %>%
  full_join(edges %>% count(to, name = "in_deg"), by = c("from" = "to")) %>%
  mutate(out_deg = replace_na(out_deg, 0),
         in_deg  = replace_na(in_deg, 0),
         deg     = out_deg + in_deg) %>%
  rename(name = from)


pick_label_nodes <- function(layout_df) {
  df <- layout_df %>%
    left_join(deg_tbl, by=c("name"="name")) %>%
    mutate(deg = replace_na(deg, 0),
           prk = phase_rank[phase],
           is_focus = element %in% ELEMENTS_TO_SHOW) %>%
    filter(class == "species" & is_focus)
  df <- df %>%
    group_by(element) %>%
    arrange(prk, desc(deg), name, .by_group = TRUE) %>%
    mutate(keep = row_number() <= MAX_LABELS_PER_ELEMENT) %>%
    ungroup()
  layout_df$keep <- FALSE
  layout_df$keep[match(df$name[df$keep], layout_df$name)] <- TRUE
  layout_df$keep
}

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
  "aq" = "#e6f5ff", "aq/sorbed" = "#e6f5ff", "aq/adsorbed"="#e6f5ff",
  "solid" = "#f2e6ff", "solid/colloid" = "#f2e6ff", "sorbed" = "#fff2cc",
  "microsite" = "#ffe6e6", "bulk" = "#e6ffe6"
)
shape_map <- c("Mobile (aq)" = 21, "Immobile/Sorbed" = 22, "Other" = 25)

# small element-based x-nudge for labels
el_nudge <- setNames(seq(-0.035, 0.035, length.out = length(ELEMENTS_TO_SHOW)), ELEMENTS_TO_SHOW)

draw_panel <- function(sub_edges, title_stub) {
  g <- igraph::graph_from_data_frame(d = sub_edges, vertices = nodes, directed = TRUE)
  lay <- ggraph::create_layout(
    g, layout = "manual",
    x = coords$x[match(igraph::V(g)$name, coords$name)],
    y = coords$y[match(igraph::V(g)$name, coords$name)]
  )
  layout_df <- as.data.frame(lay)
  keep_mask <- pick_label_nodes(layout_df)
  lab_df <- layout_df[keep_mask, c("x","y","label","element")]
  lab_df$x <- lab_df$x + (el_nudge[as.character(lab_df$element)] %||% 0)

  node_df <- layout_df[layout_df$class != "environment", ]

  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator, width = doc_weight),
                           alpha = 0.55,
                           arrow = grid::arrow(length = unit(1.6, "mm")),
                           end_cap = ggraph::circle(1.6, 'mm'),
                           show.legend = TRUE) +
    scale_edge_width(range = c(0.3, 0.9), guide = "none") +
    scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
    scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
    ggraph::geom_node_point(data = node_df,
                            aes(x = x, y = y, shape = mobility_state, fill = phase, color = element),
                            size = 2.4, stroke = 0.9, show.legend = TRUE) +
    scale_shape_manual(values = shape_map, name = "Mobility state") +
    scale_fill_manual(values = fill_map_phase, name = "Phase") +
    scale_color_manual(values = stroke_map, name = "Element (stroke)") +
    ggrepel::geom_text_repel(data = lab_df, aes(x = x, y = y, label = label),
                             size = LABEL_SIZE, min.segment.length = 0.08,
                             box.padding = 0.10, point.padding = 0.08,
                             max.overlaps = 50, force = 2, segment.size = 0.1, seed = 123) +
    coord_cartesian(xlim = c(0,1), expand = TRUE, clip = "off") +
    theme_minimal(base_size = BASE_TEXT) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = BASE_TEXT + 0.4),
          plot.margin = margin(2,2,2,2),
          legend.position = "none") +  # legend removed; exported separately
    labs(title = title_stub, x = NULL, y = NULL)
}

legend_plot <- function() {
  # Build a tiny dummy graph to extract legends
  demo_edges <- edges %>% slice_head(n = 3)
  g <- igraph::graph_from_data_frame(d = demo_edges, vertices = nodes %>% slice_head(n = 5), directed = TRUE)
  lay <- ggraph::create_layout(g, layout = "manual",
                               x = runif(igraph::gorder(g)), y = runif(igraph::gorder(g)))
  p <- ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator, width = doc_weight), show.legend = TRUE) +
    scale_edge_width(range = c(0.3, 0.9), guide = "none") +
    scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
    scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
    ggraph::geom_node_point(aes(shape = mobility_state, fill = phase, color = element), show.legend = TRUE) +
    scale_shape_manual(values = shape_map, name = "Mobility state") +
    scale_fill_manual(values = fill_map_phase, name = "Phase") +
    scale_color_manual(values = stroke_map, name = "Element (stroke)") +
    theme_void(base_size = BASE_TEXT) + theme(legend.position = "bottom")
  cowplot::get_legend(p)
}

make_doc_context_page <- function(edges_doc, ctx) {
  ph_levels  <- levels(edges_doc$pH_band)
  sub_ctx <- edges_doc %>% filter(context == ctx)
  plots <- lapply(ph_levels, function(k) {
    sub_edges <- sub_ctx %>% filter(pH_band == k)
    title_txt <- gsub(" \\(.*?\\)", "", k)
    draw_panel(sub_edges, title_stub = title_txt)
  })
  # assemble row with a left strip label
  strip <- ggplot() +
    annotate("text", x = 0, y = 0.5, label = ctx, angle = 90, vjust = 0.5, hjust = 0.5,
             size = BASE_TEXT/2.2, fontface = "bold") +
    theme_void() + theme(plot.margin = margin(0,0,0,0))
  row <- wrap_plots(c(list(strip), plots), ncol = length(plots) + 1,
                    widths = c(0.06, rep(1, length(plots))))
  row
}

for (doc in levels(edges$DOC)) {
  sub <- edges %>% filter(DOC == doc)
  row_bulk <- make_doc_context_page(sub, "Bulk oxic soil")
  row_micro <- make_doc_context_page(sub, "Anoxic microsite")
  plate <- wrap_plots(list(row_bulk, row_micro), ncol = 1) +
    plot_annotation(
      title = "Reaction network in oxic soil with anoxic microsites",
      subtitle = paste0(gsub(" \\(.*?\\)", "", doc), " • Columns = pH (acidic, circumneutral, alkaline); rows = context"),
      theme = theme(
        plot.title = element_text(face = "bold", size = BASE_TEXT + 2),
        plot.subtitle = element_text(size = BASE_TEXT + 0.6)
      )
    )
  stub <- ifelse(grepl("^Low", doc), "LOW",
                 ifelse(grepl("^Moderate", doc), "MODERATE", "HIGH"))
  ggsave(sprintf("reaction_network_byDOC_ROWSPLIT_%s.svg", stub), plate, width = FIG_W, height = FIG_H)
  ggsave(sprintf("reaction_network_byDOC_ROWSPLIT_%s.png", stub), plate, width = FIG_W, height = FIG_H, dpi = 600)
}

# Export legend only (optional for journal layouts)
leg <- legend_plot()
ggsave("legend_only_byDOC.svg", leg, width = 7, height = 1.2)
ggsave("legend_only_byDOC.png", leg, width = 7, height = 1.2, dpi = 600)

