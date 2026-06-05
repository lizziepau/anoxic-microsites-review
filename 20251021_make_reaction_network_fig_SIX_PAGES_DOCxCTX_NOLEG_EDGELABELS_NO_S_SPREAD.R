
# ===================================================================
# SIX PAGES (DOC × Context) — NO S — EDGE LABEL OVERLAP FIX
# - Spreads labels within each element row using deterministic offsets
# - Uses geom_label with a small white background for legibility
# - Keeps x-axis removed; y-axis = element symbols; numeric pH titles
# Outputs: reaction_network_MIN_NORM_PHT_NOLEG_EDGELABELS_NO_S_SPREAD_*.png/svg
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(ggraph); library(igraph);
  library(patchwork); library(ggrepel); library(stringr); library(cowplot)
})

nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7; FIG_H <- 4.167
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V")  # S excluded
BASE_TEXT <- 8.2
EDGE_LABEL_SIZE <- 1.7
EDGE_LABEL_YOFFSET <- 0.28   # base lift above arrow
LABEL_SPREAD <- 0.23         # vertical spread step within a row
ROW_PADDING  <- 0.45         # keep labels within +/- this from the row center

# ----- factors & weights -----
edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$DOC     <- factor(edges$DOC,     levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- as.numeric(edges$doc_weight)

# ---- Process class (linetype) ----
edges <- edges %>% mutate(
  mediator_class = dplyr::case_when(
    mediator %in% c("abiotic","O₂") ~ "solid",
    mediator %in% c("abiotic/biotic","Fe(II)/sulfide/OM","Fe(II)/sulfide",
                    "Mn(IV) oxides","O₂/Mn oxides","carbonate","Mn/Fe oxides",
                    "O₂/pH","O₂/NO₃⁻","carbonate/OM/O₂") ~ "dashed",
    mediator %in% c("microbial","microbial/OM","microbial/Fe(II)",
                    "microbial/Fe(II)/sulfide","Fe(II)/sulfide/bioreduction") ~ "dotted",
    TRUE ~ "dashed"
  )
)

# ---- ELEMENT normalization ----
nodes$element[is.na(nodes$element)] <- "default"
nodes$element <- dplyr::case_when(nodes$element %in% c("Vanadium","V(V)") ~ "V", TRUE ~ nodes$element)

# ---- NAME NORMALIZATION KEYS ----
norm_key <- function(x) {
  x %>%
    str_replace_all("₀","0") %>% str_replace_all("₁","1") %>% str_replace_all("₂","2") %>%
    str_replace_all("₃","3") %>% str_replace_all("₄","4") %>% str_replace_all("₅","5") %>%
    str_replace_all("₆","6") %>% str_replace_all("₇","7") %>% str_replace_all("₈","8") %>%
    str_replace_all("₉","9") %>%
    str_replace_all("²","^2") %>% str_replace_all("³","^3") %>%
    str_replace_all("−","-") %>% str_replace_all("–","-") %>% str_replace_all("—","-") %>%
    str_replace_all("’","'") %>%
    str_replace_all("SO₄","SO4") %>% str_replace_all("H₂S","H2S") %>%
    str_replace_all("\\s*\\(\\s*","(") %>% str_replace_all("\\s*\\)\\s*",")") %>%
    str_squish() %>% tolower()
}
nodes <- nodes %>% mutate(key = norm_key(name))
edges <- edges %>% mutate(from_key = norm_key(from), to_key = norm_key(to))

edges <- edges %>%
  left_join(nodes %>% select(key, from_name=name), by=c("from_key"="key")) %>%
  left_join(nodes %>% select(key, to_name=name),   by=c("to_key"="key")) %>%
  mutate(from = coalesce(from_name, from), to = coalesce(to_name, to)) %>%
  select(-from_name,-to_name)

# ---- Keep focus elements (S excluded) + anchors ----
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter((element %in% keep_el) | name %in% c("Bulk oxic soil","Anoxic microsite"))

# ---- Phase normalization ----
nodes <- nodes %>%
  mutate(
    phase = coalesce(phase, ""),
    phase_norm = dplyr::case_when(
      str_detect(phase, regex("aq", TRUE)) ~ "aq",
      str_detect(phase, regex("sorb", TRUE)) ~ "sorbed",
      str_detect(phase, regex("solid|oxide|colloid", TRUE)) ~ "solid/colloid",
      str_detect(phase, regex("microsite", TRUE)) ~ "microsite",
      str_detect(phase, regex("bulk", TRUE)) ~ "bulk",
      TRUE ~ "sorbed"
    ),
    mobility_state = dplyr::case_when(
      phase_norm == "aq" ~ "Mobile (aq)",
      phase_norm %in% c("sorbed","solid/colloid") ~ "Immobile/Sorbed",
      TRUE ~ "Other"
    )
  )

# Filter edges to retained nodes
edges <- edges %>% filter(from %in% nodes$name & to %in% nodes$name)

# ---- Lanes (x) ----
lane_map <- c("solid/colloid"=0.08, "sorbed"=0.16, "aq"=0.50, "microsite"=0.98, "bulk"=0.02)
nodes <- nodes %>% mutate(lane = ifelse(phase_norm %in% names(lane_map), lane_map[phase_norm], 0.50)) %>%
  mutate(lane = dplyr::case_when(name=="Bulk oxic soil" ~ 0.02, name=="Anoxic microsite" ~ 0.98, TRUE ~ lane))

# ---- y positions (robust) ----
el_levels <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V","default")
nodes <- nodes %>%
  mutate(element = ifelse(is.na(element) | element == "", "default", element)) %>%
  mutate(element = factor(element, levels = el_levels),
         ypos = match(as.character(element), el_levels))
coords <- nodes %>% transmute(name, x=lane, y=ypos)

# ---- Simplified labels ----
phase_tag <- function(pn) dplyr::case_when(pn=="aq"~"(aq)", pn %in% c("solid/colloid","sorbed","microsite")~"(s)", TRUE~"")
simplify_formula <- function(s) {
  s <- trimws(s)
  repl <- list(
    "Fe\\(III\\) \\(ferrihydrite/goethite\\)"="Fe(III)","Fe\\(II\\) \\(aq\\)"="Fe(II)","FeS \\(mackinawite/pyrite\\)"="FeS",
    "Siderite \\(FeCO3\\)"="FeCO3","Fe\\(III\\) oxides"="Fe(III)Ox","Mn\\(IV\\) oxides \\(birnessite\\)"="Mn(IV)Ox","Mn\\(IV\\) oxides"="Mn(IV)Ox",
    "Mn\\(II\\) \\(aq\\)"="Mn(II)","As\\(V\\) \\(arsenate\\) sorbed"="As(V)","As\\(III\\) \\(arsenite\\) \\(aq\\)"="As(III)",
    "As2S3 \\(orpiment\\)"="As2S3","Sb\\(V\\) \\(antimonate\\) sorbed"="Sb(V)","Sb\\(III\\) \\(aq\\)"="Sb(III)","Sb2S3 \\(stibnite\\)"="Sb2S3",
    "Cr\\(VI\\) \\(chromate\\) \\(aq\\)"="CrO4^2-","Cr\\(III\\) \\(hydroxide/oxyhydroxide\\) \\(s\\)"="Cr(III)OH",
    "Tc\\(VII\\) \\(TcO4\\^−\\) \\(aq\\)"="TcO4^-","Tc\\(IV\\) \\(TcO2·nH2O\\) \\(s\\)"="TcO2·nH2O",
    "Pu\\(V/VI\\) \\(aq/complexed\\)"="Pu(V/VI)","Pu\\(IV\\) hydroxo/oxide \\(s/colloid\\)"="Pu(IV)Ox",
    "U\\(VI\\)–carbonate complexes \\(aq\\)"="U(VI)-CO3","U\\(IV\\) \\(UO2\\) \\(s\\)"="UO2",
    "Se\\(VI\\)/\\(IV\\) oxyanions \\(aq/sorbed\\)"="Se(VI/IV)","Se\\(0\\) \\(s\\)"="Se(0)",
    "FeSe \\(s\\)"="FeSe","V\\(V\\) \\(vanadate\\).*"="V(V)","V\\(IV/III\\).*"="V(IV/III)",
    "Organic ligands \\(OM\\)"="OM"
  )
  for (pat in names(repl)) s <- gsub(pat, repl[[pat]], s, perl=TRUE)
  s <- gsub("\\s+"," ", s); trimws(s)
}
nodes <- nodes %>%
  mutate(label_simple = paste0(simplify_formula(name), " ", phase_tag(phase_norm)) %>%
           trimws() %>%
           str_replace_all("\\((aq|s|g)\\)\\s*\\((\\1)\\)", "(\\1)") %>%
           str_replace_all("\\s*\\((aq|s|g)\\)\\s*\\1", "(\\1)") %>%
           stringr::str_squish())

# ---- Aesthetics ----
stroke_map <- c(Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
                Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666")
col_mob <- c(mobilize="#3b5b92", immobilize="#a35e10", neutral="#7a7a7a")
fill_map_phase <- c(aq="#e6f5ff", sorbed="#fff2cc", `solid/colloid`="#f2e6ff", microsite="#ffe6e6", bulk="#e6ffe6")
shape_map <- c("Mobile (aq)"=21, "Immobile/Sorbed"=22, "Other"=25)

# ---- Helper to build edge label map with vertical spreading ----
make_label_map <- function(sub_edges) {
  df <- sub_edges %>%
    mutate(x = coords$x[match(from, coords$name)], y = coords$y[match(from, coords$name)],
           xend = coords$x[match(to, coords$name)], yend = coords$y[match(to, coords$name)]) %>%
    mutate(xm = (x + xend)/2, ym = (y + yend)/2 + EDGE_LABEL_YOFFSET,
           row = round((y + yend)/2),
           lab_from = nodes$label_simple[match(from, nodes$name)],
           lab_to   = nodes$label_simple[match(to,   nodes$name)],
           edge_lab = paste0(lab_from, " \u2192 ", lab_to)) %>%
    group_by(row) %>% arrange(xm, .by_group = TRUE) %>%
    mutate(idx = row_number(), nlab = n(),
           offset = (idx - (nlab + 1)/2) * LABEL_SPREAD,
           ym_adj = pmin(pmax(ym + offset, row - ROW_PADDING), row + ROW_PADDING)) %>%
    ungroup()
  df
}

# ---- Draw panel ----
draw_panel <- function(sub_edges, title_stub) {
  lab_map <- make_label_map(sub_edges)
  g <- igraph::graph_from_data_frame(d = sub_edges, vertices = nodes, directed = TRUE)
  lay <- ggraph::create_layout(g, layout = "manual",
                               x = coords$x[match(igraph::V(g)$name, coords$name)],
                               y = coords$y[match(igraph::V(g)$name, coords$name)])
  layout_df <- as.data.frame(lay)
  node_df <- layout_df[layout_df$class == "species", ]

  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator_class, width = doc_weight),
                           alpha = 0.55, arrow = grid::arrow(length = unit(1.4, "mm")),
                           end_cap = ggraph::circle(1.4, 'mm'), show.legend = FALSE) +
    scale_edge_width(range = c(0.28, 0.85), guide = "none") +
    scale_edge_color_manual(values = col_mob, guide = "none") +
    scale_edge_linetype_manual(values = c(solid = 1, dashed = 2, dotted = 3), guide = "none") +
    ggraph::geom_node_point(data = node_df,
                            aes(x = x, y = y, shape = mobility_state, fill = phase_norm, color = element),
                            size = 2.3, stroke = 0.9, show.legend = FALSE) +
    scale_shape_manual(values = shape_map, guide = "none") +
    scale_fill_manual(values = fill_map_phase, guide = "none") +
    scale_color_manual(values = stroke_map, guide = "none") +
    # label background
    geom_label(data = lab_map, aes(x = xm, y = ym_adj, label = edge_lab),
               size = EDGE_LABEL_SIZE, label.padding = unit(0.05, "lines"),
               label.size = 0, alpha = 0.85, fill = "white") +
    coord_cartesian(xlim = c(0,1), expand = TRUE, clip = "off") +
    theme_minimal(base_size = BASE_TEXT) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = BASE_TEXT + 0.4),
          plot.margin = margin(2,2,2,2),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
          axis.title.y = element_blank(), legend.position = "none") +
    scale_y_continuous(breaks = sort(unique(nodes$ypos)),
                       labels = as.character(nodes$element)[match(sort(unique(nodes$ypos)), nodes$ypos)],
                       expand = expansion(mult = c(0.02, 0.02))) +
    labs(title = title_stub)
}

make_page <- function(edges_doc_ctx, ctx_label, doc_label) {
  ph_levels  <- levels(edges_doc_ctx$pH_band)
  title_map <- c("acidic (4.5–6.0)"="Acidic (pH 4.5–6.0)",
                 "circumneutral (6.0–7.5)"="Circumneutral (pH 6.0–7.5)",
                 "alkaline (7.5–8.5)"="Alkaline (pH 7.5–8.5)")
  plots <- lapply(ph_levels, function(k) {
    sub_edges <- edges_doc_ctx %>% filter(pH_band == k)
    title_txt <- unname(title_map[[as.character(k)]] %||% as.character(k))
    draw_panel(sub_edges, title_stub = title_txt)
  })
  strip <- ggplot() + annotate("text", x = 0, y = 0.5, label = ctx_label, angle = 90, vjust = 0.5, hjust = 0.5,
             size = BASE_TEXT/2, fontface = "bold") + theme_void() + theme(plot.margin = margin(0,0,0,0))
  row <- patchwork::wrap_plots(plotlist = c(list(strip), plots), ncol = length(plots) + 1, widths = c(0.06, rep(1, length(plots))))
  row + patchwork::plot_annotation(
      title = "Reaction network in oxic soil with anoxic microsites",
      subtitle = paste0(gsub(" \\(.*?\\)", "", doc_label),
                        " • Columns = pH bands with numeric ranges • Labels = reactant \u2192 product above arrows (auto-spread)"),
      theme = theme(plot.title = element_text(face = "bold", size = BASE_TEXT + 2),
                    plot.subtitle = element_text(size = BASE_TEXT + 0.6),
                    legend.position = "none"))
}

`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

for (doc in levels(edges$DOC)) {
  for (ctx in levels(edges$context)) {
    sub <- edges %>% filter(DOC == doc, context == ctx)
    plate <- make_page(sub, ctx_label = ctx, doc_label = doc)
    stub_doc <- ifelse(grepl("^Low", doc), "LOW", ifelse(grepl("^Moderate", doc), "MODERATE", "HIGH"))
    stub_ctx <- ifelse(grepl("^Bulk", ctx), "BULK", "MICRO")
    ggsave(sprintf("reaction_network_MIN_NORM_PHT_NOLEG_EDGELABELS_NO_S_SPREAD_%s_%s.svg", stub_doc, stub_ctx), plate, width = FIG_W, height = FIG_H)
    ggsave(sprintf("reaction_network_MIN_NORM_PHT_NOLEG_EDGELABELS_NO_S_SPREAD_%s_%s.png", stub_doc, stub_ctx), plate, width = FIG_W, height = FIG_H, dpi = 600)
  }
}

