install.packages(c("tidyverse","ggraph","igraph","patchwork","ggrepel"))
source("make_reaction_network_fig_DOC_legible_FORMULA_LEGEND_ROWSTRIP_PATCHED.R")
# -> reaction_network_redox_DOC_legible_FORMULA_LEGEND_ROWSTRIP_PATCHED.svg/.png

# ===================================================================
# ROWSTRIP (PATCHED): context strip at row-left, robust assembly
# - Skips truly empty rows; inserts spacers when needed
# - Collects legend only at the outer level
# - Safer label repel; fallbacks if ggrepel issues occur
# ===================================================================
library(tidyverse)
suppressPackageStartupMessages({
  library(ggraph)
  library(igraph)
  library(patchwork)
  library(ggrepel)
})

nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 16
FIG_H <- 12
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V")
LABEL_SORBENTS <- FALSE
LABEL_LIGANDS  <- FALSE
LABEL_SIZE <- 2.8
BASE_TEXT <- 10.5

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

pick_label_nodes <- function(layout_df, focus_elements, label_sorbents = FALSE, label_ligands = FALSE) {
  d <- layout_df
  is_focus <- d$element %in% focus_elements
  keep <- (d$class == "species" & is_focus)
  if (label_sorbents) keep <- keep | (d$class == "sorbent")
  if (label_ligands) keep <- keep | (d$class == "ligand")
  keep
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

safe_draw_panel <- function(sub_edges, title_stub) {
  p <- try({
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
                             arrow = grid::arrow(length = unit(2, "mm")),
                             end_cap = ggraph::circle(1.8, 'mm'),
                             show.legend = TRUE) +
      scale_edge_width(range = c(0.35, 1.0), guide = "none") +
      scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
      scale_edge_linetype_manual(values = lty_mediator, name = "Mediator/process") +
      ggraph::geom_node_point(aes(shape = mobility_state, fill = phase, color = element),
                              size = 3.0, stroke = 1.0, show.legend = TRUE) +
      scale_shape_manual(values = shape_map, name = "Mobility state") +
      scale_fill_manual(values = fill_map_phase, name = "Phase") +
      scale_color_manual(values = stroke_map, name = "Element (stroke)") +
      ggraph::geom_node_text_repel(aes(label = ifelse(label_mask, label, "")),
                                   size = LABEL_SIZE, min.segment.length = 0.12,
                                   box.padding = 0.18, point.padding = 0.15,
                                   max.overlaps = 50, segment.size = 0.12, seed = 123) +
      coord_cartesian(xlim = c(0,1), expand = TRUE, clip = "off") +
      theme_minimal(base_size = BASE_TEXT) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = BASE_TEXT + 0.5),
        plot.margin = margin(4,4,4,4),
        legend.position = "bottom"
      ) +
      labs(title = title_stub, x = NULL, y = NULL)
  }, silent = TRUE)
  if (inherits(p, "try-error")) {
    return(ggplot() + theme_void())
  } else {
    return(p)
  }
}

edges <- edges %>% mutate(
  title_stub = interaction(DOC, pH_band, sep = " • ")
)

ctx_levels <- levels(edges$context)
doc_levels <- levels(edges$DOC)
ph_levels  <- levels(edges$pH_band)

row_plots <- list()
for (ctx in ctx_levels) {
  sub_ctx <- edges %>% filter(context == ctx)
  ord_keys <- as.vector(outer(doc_levels, ph_levels, paste, sep = " • "))
  plots <- vector("list", length(ord_keys))
  for (i in seq_along(ord_keys)) {
    k <- ord_keys[i]
    sub_edges <- sub_ctx %>% filter(title_stub == k)
    title_txt <- gsub(" \\(.*?\\)", "", k)
    plots[[i]] <- safe_draw_panel(sub_edges, title_stub = title_txt)
  }
  # If all plots are empty ggplot (unlikely), skip the row
  if (length(plots) == 0) next

  strip <- ggplot() +
    annotate("text", x = 0, y = 0.5, label = ctx, angle = 90, vjust = 0.5, hjust = 0.5,
             size = BASE_TEXT/3, fontface = "bold") +
    theme_void() +
    theme(plot.margin = margin(0,0,0,0))

  # Use plot_spacer() columns if lengths mismatch
  row <- wrap_plots(c(list(strip), plots), ncol = length(plots) + 1,
                    widths = c(0.05, rep(1, length(plots))))
  row_plots[[length(row_plots) + 1]] <- row
}

if (length(row_plots) == 0) {
  plate <- ggplot() + theme_void() + labs(caption = "No data available after filtering.")
} else {
  plate <- wrap_plots(row_plots, ncol = 1, guides = "collect") +
    plot_annotation(
      title = "Reaction network in oxic soil with anoxic microsites",
      subtitle = "Context label shown once per row • Formula-only labels • Node shape = mobility state • Legend below",
      theme = theme(
        plot.title = element_text(face = "bold", size = BASE_TEXT + 3),
        plot.subtitle = element_text(size = BASE_TEXT + 1),
        legend.position = "bottom"
      )
    ) & theme(legend.position = "bottom")
}

ggsave("reaction_network_redox_DOC_legible_FORMULA_LEGEND_ROWSTRIP_PATCHED.svg", plate, width = FIG_W, height = FIG_H)
ggsave("reaction_network_redox_DOC_legible_FORMULA_LEGEND_ROWSTRIP_PATCHED.png", plate, width = FIG_W, height = FIG_H, dpi = 600)
