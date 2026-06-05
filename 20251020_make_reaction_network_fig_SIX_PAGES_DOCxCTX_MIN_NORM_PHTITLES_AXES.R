
# ===================================================================
# SIX PAGES (DOC × Context) — MINIMAL & LEGIBLE
# Phase normalization + numeric pH titles + DESCRIPTIVE AXES
# Output size: 7.0" × 4.167"
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggraph)
  library(igraph)
  library(patchwork)
  library(ggrepel)
  library(stringr)
})

# ---- Load data ----
nodes <- read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ---- Figure sizing ----
FIG_W <- 7
FIG_H <- 4.167

# ---- Controls ----
ELEMENTS_TO_SHOW <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V")
MAX_LABELS_PER_ELEMENT <- 1
LABEL_SIZE <- 2.1
BASE_TEXT <- 8.2

# ---- Factor levels ----
edges$pH_band <- factor(edges$pH_band, levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"))
edges$DOC     <- factor(edges$DOC,     levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- as.numeric(edges$doc_weight)

# ---- Normalize elements (ensure stroke color mapping) ----
nodes$element[is.na(nodes$element)] <- "default"
nodes$element <- dplyr::case_when(
  nodes$element %in% c("Vanadium", "V(V)") ~ "V",
  TRUE ~ nodes$element
)

# Keep only focus elements + default
keep_el <- c(ELEMENTS_TO_SHOW, "default")
nodes <- nodes %>% filter(element %in% keep_el)

# ---- Phase normalization ----
nodes <- nodes %>%
  mutate(
    phase = coalesce(phase, ""),
    phase_norm = case_when(
      str_detect(phase, regex("aq", ignore_case = TRUE)) ~ "aq",
      str_detect(phase, regex("sorb", ignore_case = TRUE)) ~ "sorbed",
      str_detect(phase, regex("solid|oxide|colloid", ignore_case = TRUE)) ~ "solid/colloid",
      str_detect(phase, regex("microsite", ignore_case = TRUE)) ~ "microsite",
      str_detect(phase, regex("bulk", ignore_case = TRUE)) ~ "bulk",
      TRUE ~ "sorbed" # safe default
    )
  )

# ---- Mobility state from normalized phase ----
nodes <- nodes %>% mutate(
  mobility_state = case_when(
    phase_norm == "aq" ~ "Mobile (aq)",
    phase_norm %in% c("sorbed","solid/colloid") ~ "Immobile/Sorbed",
    TRUE ~ "Other"
  )
)

# ---- Filter species nodes only (reduce clutter) ----
nodes <- nodes %>% filter(class == "species" | name %in% c("Bulk oxic soil","Anoxic microsite"))
edges <- edges %>% filter(from %in% nodes$name & to %in% nodes$name)

# ---- Lanes (x positions) ----
lane_map <- c("solid/colloid"=0.08, "sorbed"=0.16, "aq"=0.50, "microsite"=0.98, "bulk"=0.02)
default_lane <- 0.50
nodes <- nodes %>% mutate(lane = ifelse(phase_norm %in% names(lane_map), lane_map[phase_norm], default_lane))

# Keep explicit anchors for context names if present
nodes <- nodes %>% mutate(lane = case_when(
  name=="Bulk oxic soil" ~ 0.02,
  name=="Anoxic microsite" ~ 0.98,
  TRUE ~ lane
))

# ---- y positions by element row ----
el_levels <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","S","Sb","V","default")
nodes$element <- factor(nodes$element, levels = el_levels)
y_map <- nodes %>% distinct(element) %>% arrange(element) %>% mutate(ypos = row_number())
nodes <- nodes %>% left_join(y_map, by="element")
coords <- nodes %>% transmute(name, x=lane, y=ypos)

# ---- Formula-only labels ----
shorten_label <- function(s) {
  s <- trimws(s)
  repl <- list(
    "Fe\\(III\\) \\(ferrihydrite/goethite\\)" = "Fe(III)",
    "Fe\\(II\\) \\(aq\\)" = "Fe(II)_aq",
    "FeS \\(mackinawite/pyrite\\)" = "FeS",
    "Siderite \\(FeCO3\\)" = "FeCO3(s)",
    "Fe\\(III\\) oxides" = "Fe(III)Ox(s)",
    "Mn\\(IV\\) oxides \\(birnessite\\)" = "Mn(IV)Ox(s)",
    "Mn\\(IV\\) oxides" = "Mn(IV)Ox(s)",
    "Mn\\(II\\) \\(aq\\)" = "Mn(II)_aq",
    "As\\(V\\) \\(arsenate\\) sorbed" = "As(V)_sorbed",
    "As\\(III\\) \\(arsenite\\) \\(aq\\)" = "As(III)_aq",
    "As2S3 \\(orpiment\\)" = "As2S3(s)",
    "Sb\\(V\\) \\(antimonate\\) sorbed" = "Sb(V)_sorbed",
    "Sb\\(III\\) \\(aq\\)" = "Sb(III)_aq",
    "Sb2S3 \\(stibnite\\)" = "Sb2S3(s)",
    "Cr\\(VI\\) \\(chromate\\) \\(aq\\)" = "CrO4^2-(aq)",
    "Cr\\(III\\) \\(hydroxide/oxyhydroxide\\) \\(s\\)" = "Cr(III)OH(s)",
    "Tc\\(VII\\) \\(TcO4\\^−\\) \\(aq\\)" = "TcO4^-(aq)",
    "Tc\\(IV\\) \\(TcO2·nH2O\\) \\(s\\)" = "TcO2·nH2O(s)",
    "Pu\\(V/VI\\) \\(aq/complexed\\)" = "Pu(V/VI)_aq",
    "Pu\\(IV\\) hydroxo/oxide \\(s/colloid\\)" = "Pu(IV)_ox(s/col)",
    "U\\(VI\\)–carbonate complexes \\(aq\\)" = "U(VI)-CO3(aq)",
    "U\\(IV\\) \\(UO2\\) \\(s\\)" = "UO2(s)",
    "Sulfate \\(SO4\\^2−\\)" = "SO4^2-",
    "HS−/H2S" = "HS-/H2S",
    "Se\\(VI\\)/\\(IV\\) oxyanions \\(aq/sorbed\\)" = "Se(VI/IV)_aq/sorb",
    "Se\\(0\\) \\(s\\)" = "Se(0)(s)",
    "FeSe \\(s\\)" = "FeSe(s)",
    "V\\(V\\) \\(vanadate\\) \\(aq/sorbed\\)" = "V(V)_aq/sorb",
    "V\\(IV/III\\) \\(adsorbed/oxide\\)" = "V(IV/III)_ox/ads",
    "Organic ligands \\(OM\\)" = "OM",
    "Sulfide \\(S2−/HS−\\)" = "S2-/HS-"
  )
  for (pat in names(repl)) s <- gsub(pat, repl[[pat]], s, perl = TRUE)
  s <- gsub("\\s+", " ", s); trimws(s)
}
nodes$label <- vapply(nodes$name, shorten_label, character(1))

# ---- Degree proxy (no reserved words) ----
deg_tbl <- edges %>%
  count(from, name = "out_deg") %>%
  full_join(edges %>% count(to, name = "in_deg"), by = c("from" = "to")) %>%
  mutate(out_deg = replace_na(out_deg, 0),
         in_deg  = replace_na(in_deg, 0),
         deg     = out_deg + in_deg) %>%
  rename(name = from)

# ---- Prefer aqueous labels ----
phase_rank <- c("aq"=1, "sorbed"=2, "solid/colloid"=3, "bulk"=4, "microsite"=4)

pick_label_nodes <- function(layout_df) {
  df <- layout_df %>%
    left_join(deg_tbl, by = c("name"="name")) %>%
    mutate(deg = replace_na(deg, 0),
           prk = phase_rank[phase_norm],
           is_focus = element %in% ELEMENTS_TO_SHOW) %>%
    filter(class == "species" & is_focus) %>%
    filter(phase_norm %in% c("aq","sorbed"))  # only label aq/sorbed
  df <- df %>%
    group_by(element) %>%
    arrange(prk, desc(deg), name, .by_group = TRUE) %>%
    mutate(keep = row_number() <= MAX_LABELS_PER_ELEMENT) %>%
    ungroup()
  layout_df$keep <- FALSE
  layout_df$keep[match(df$name[df$keep], layout_df$name)] <- TRUE
  layout_df$keep
}

# ---- Aesthetics ----
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
  "aq" = "#e6f5ff",
  "sorbed" = "#fff2cc",
  "solid/colloid" = "#f2e6ff",
  "microsite" = "#ffe6e6",
  "bulk" = "#e6ffe6"
)
shape_map <- c("Mobile (aq)" = 21, "Immobile/Sorbed" = 22, "Other" = 25)

# small x-nudge per element to reduce label stacking
el_nudge <- setNames(seq(-0.035, 0.035, length.out = length(ELEMENTS_TO_SHOW)), ELEMENTS_TO_SHOW)

# ---- Draw one pH panel ----
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
  nz <- el_nudge[as.character(lab_df$element)]; nz[is.na(nz)] <- 0
  lab_df$x <- lab_df$x + nz

  node_df <- layout_df[layout_df$class == "species", ]

  ggraph::ggraph(lay) +
    ggraph::geom_edge_link(aes(color = mobility_effect, linetype = mediator, width = doc_weight),
                           alpha = 0.55,
                           arrow = grid::arrow(length = unit(1.4, "mm")),
                           end_cap = ggraph::circle(1.4, 'mm'),
                           show.legend = TRUE) +
    scale_edge_width(range = c(0.28, 0.85), guide = "none") +
    scale_edge_color_manual(values = col_mob, name = "Effect on mobility") +
    scale_edge_linetype_manual(values = lty_mediator, guide = "none") +   # mediator legend hidden
    ggraph::geom_node_point(data = node_df,
                            aes(x = x, y = y, shape = mobility_state, fill = phase_norm, color = element),
                            size = 2.3, stroke = 0.9, show.legend = TRUE) +
    scale_shape_manual(values = shape_map, name = "Mobility state") +
    scale_fill_manual(values = fill_map_phase, guide = "none") +          # phase legend hidden
    scale_color_manual(values = stroke_map, guide = "none") +             # element legend hidden

    # ---- DESCRIPTIVE AXES ----
 # scale_x_continuous(
  #  breaks = if (unique(sub_edges$context) == "Bulk oxic soil")
   #   c(0.08, 0.50) else c(0.16, 0.50, 0.98),
    #labels = if (unique(sub_edges$context) == "Bulk oxic soil")
     # c("Sorbent phases", "Aqueous phase")
    #else
     # c("Sorbent phases", "Aqueous phase", "Reduced solids"),
    #expand = expansion(mult = c(0.02, 0.02))
#  ) +
    scale_y_continuous(
      breaks = y_map$ypos,
      labels = y_map$element,
      expand = expansion(mult = c(0.02, 0.02))
    ) +

    ggrepel::geom_text_repel(data = lab_df, aes(x = x, y = y, label = label),
                             size = LABEL_SIZE, min.segment.length = 0.06,
                             box.padding = 0.10, point.padding = 0.08,
                             max.overlaps = 40, force = 2, segment.size = 0.08, seed = 123) +
    coord_cartesian(xlim = c(0,1), expand = TRUE, clip = "off") +
    theme_minimal(base_size = BASE_TEXT) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = margin(2, 2, 2, 2),
      legend.position = "bottom"
    ) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = BASE_TEXT + 0.4),
          plot.margin = margin(2,2,2,2),
          axis.title.x = element_text(margin = margin(t = 4)),
          axis.title.y = element_text(margin = margin(r = 4)),
          legend.position = "bottom") +
    labs(
      title = title_stub,
      x = NULL,
      y = "Element"
    )
}

# ---- Build a DOC×Context page (1×3 pH columns) with numeric pH titles ----
make_page <- function(edges_doc_ctx, ctx_label, doc_label) {
  ph_levels  <- levels(edges_doc_ctx$pH_band)
  title_map <- c(
    "acidic (4.5–6.0)"        = "Acidic (pH 4.5–6.0)",
    "circumneutral (6.0–7.5)" = "Circumneutral (pH 6.0–7.5)",
    "alkaline (7.5–8.5)"      = "Alkaline (pH 7.5–8.5)"
  )
  plots <- lapply(ph_levels, function(k) {
    sub_edges <- edges_doc_ctx %>% filter(pH_band == k)
    title_txt <- unname(title_map[[as.character(k)]] %||% as.character(k))
    draw_panel(sub_edges, title_stub = title_txt)
  })

  # left context strip
  strip <- ggplot() +
    annotate("text", x = 0, y = 0.5, label = ctx_label, angle = 90, vjust = 0.5, hjust = 0.5,
             size = BASE_TEXT/2, fontface = "bold") +
    theme_void() + theme(plot.margin = margin(0,0,0,0))

  row <- patchwork::wrap_plots(plotlist = c(list(strip), plots),
                               ncol = length(plots) + 1,
                               widths = c(0.06, rep(1, length(plots))))

  row +
    patchwork::plot_annotation(
      title = "Reaction network in oxic soil with anoxic microsites",
      subtitle = paste0(gsub(" \\(.*?\\)", "", doc_label),
                        " • Columns = pH bands with numeric ranges • Legends: mobility-only"),
      theme = theme(
        plot.title = element_text(face = "bold", size = BASE_TEXT + 2),
        plot.subtitle = element_text(size = BASE_TEXT + 0.6),
        legend.position = "bottom"
      )
    )
}

# Helper for %||% (avoid importing rlang just for this)
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

# ---- Emit SIX pages: DOC × context ----
for (doc in levels(edges$DOC)) {
  for (ctx in levels(edges$context)) {
    sub <- edges %>% filter(DOC == doc, context == ctx)
    plate <- make_page(sub, ctx_label = ctx, doc_label = doc)
    stub_doc <- ifelse(grepl("^Low", doc), "LOW",
                 ifelse(grepl("^Moderate", doc), "MODERATE", "HIGH"))
    stub_ctx <- ifelse(grepl("^Bulk", ctx), "BULK", "MICRO")
    ggsave(sprintf("reaction_network_MIN_NORM_PHT_AXES_%s_%s.svg", stub_doc, stub_ctx), plate, width = FIG_W, height = FIG_H)
    ggsave(sprintf("reaction_network_MIN_NORM_PHT_AXES_%s_%s.png", stub_doc, stub_ctx), plate, width = FIG_W, height = FIG_H, dpi = 600)
  }
}

