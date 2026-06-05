
# ===================================================================
# HEATMAP — THREE PANELS BY DOC (one page per DOC level) WITH SUBHEADING
# File: make_concept_heatmap_network_S_VERTICAL_byDOC_BLACKTEXT.R
# - Title: "Reaction network in bulk oxic soil with anoxic microsites"
# - Adds a bold subheading above the facet strip indicating the DOC level
# - Keeps: black text; two-line pH labels; numeric DOC ranges; vertical arrows; Process legend "↑ • ↓"
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales); library(cowplot)
})

# ---- IO ----
nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ---- Figure size for single panels ----
FIG_W <- 7; FIG_H <- 4.167

# ---- Elements ----
ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V","S")

# ---- Title ----
#PANEL_TITLE <- "Reaction network in bulk oxic soil with anoxic microsites"

# ---- Palettes ----
FILL_LOW  <- "#E1341E"; FILL_MID  <- "#e8e8e8"; FILL_HIGH <- "#1ECBE1"
proc_col  <- c(Abiotic="#961EE1", `Mineral-mediated`="goldenrod", Microbial="green4")

# Thin space (U+202F) for units
nbsp_thin <- "\u202F"

# ---- Factor normalization ----
edges$pH_band <- factor(edges$pH_band,
  levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"),
  labels = c("Acidic\n(pH 4.5–6.0)", "Circumneutral\n(pH 6.0–7.5)", "Alkaline\n(pH 7.5–8.5)")
)

# DOC labels with scientific-style units
doc_labels <- c(
  paste0("Low DOC (<", nbsp_thin, "2 mg", nbsp_thin, "C", nbsp_thin, "L\u207B\u00B9)"),
  paste0("Moderate DOC (2–10 mg", nbsp_thin, "C", nbsp_thin, "L\u207B\u00B9)"),
  paste0("High DOC (>", nbsp_thin, "10 mg", nbsp_thin, "C", nbsp_thin, "L\u207B\u00B9)")
)
edges$DOC <- factor(edges$DOC,
  levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"),
  labels = doc_labels
)

edges$context <- factor(edges$context, levels=c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- suppressWarnings(as.numeric(edges$doc_weight))

# ---- Normalize node elements ----
nodes$element <- ifelse(is.na(nodes$element)|nodes$element=="","default",nodes$element)
nodes$element[nodes$element %in% c("Vanadium","V(V)")] <- "V"

# ---- Map mediator -> 3 process classes ----
edges <- edges %>% mutate(
  process = case_when(
    mediator %in% c("abiotic","O₂","O2") ~ "Abiotic",
    mediator %in% c("Mn(IV) oxides","Mn/Fe oxides","O₂/Mn oxides","carbonate","O₂/pH","O₂/NO₃⁻","carbonate/OM/O₂") ~ "Mineral-mediated",
    mediator %in% c("microbial","microbial/OM","microbial/Fe(II)","microbial/Fe(II)/sulfide","Fe(II)/sulfide/bioreduction") ~ "Microbial",
    mediator %in% c("abiotic/biotic","Fe(II)/sulfide/OM","Fe(II)/sulfide") ~ "Mineral-mediated",
    TRUE ~ "Mineral-mediated"
  )
)

# ---- Net mobility score ----
edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- dplyr::case_when(
  edges$mobility_effect == "mobilize"   ~ +1 * edges$w,
  edges$mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

# ---- Map edge names to elements ----
norm_key <- function(x){
  x %>% str_replace_all("₀","0") %>% str_replace_all("₁","1") %>% str_replace_all("₂","2") %>%
    str_replace_all("₃","3") %>% str_replace_all("₄","4") %>% str_replace_all("₅","5") %>%
    str_replace_all("₆","6") %>% str_replace_all("₇","7") %>% str_replace_all("₈","8") %>%
    str_replace_all("₉","9") %>% str_squish() %>% tolower()
}
nodes$key <- norm_key(nodes$name)
edges$from_key <- norm_key(edges$from)
edges$to_key   <- norm_key(edges$to)
map_el <- nodes %>% select(key, element) %>% mutate(element = ifelse(is.na(element),"default",element))
edges <- edges %>%
  left_join(map_el, by=c("from_key"="key")) %>% rename(from_el = element) %>%
  left_join(map_el, by=c("to_key"="key"))   %>% rename(to_el   = element)
edges$element <- ifelse(edges$from_el %in% ELEMENTS, edges$from_el,
                        ifelse(edges$to_el %in% ELEMENTS, edges$to_el, "default"))
edges_filt <- edges %>% filter(element %in% ELEMENTS)

# ---- Aggregate to cells ----
cell <- edges_filt %>%
  group_by(DOC, pH_band, context, element) %>%
  summarise(
    net = sum(score, na.rm=TRUE),
    dom = names(which.max(c(
            Abiotic = sum(process=="Abiotic"),
            `Mineral-mediated` = sum(process=="Mineral-mediated"),
            Microbial = sum(process=="Microbial")
          ))),
    .groups = "drop"
  )

# ---- Complete grid ----
all_cells <- expand.grid(
  DOC = levels(edges$DOC),
  pH_band = levels(edges$pH_band),
  context = levels(edges$context),
  element = ELEMENTS
) %>% as_tibble()
cell <- all_cells %>% left_join(cell, by=c("DOC","pH_band","context","element")) %>%
  mutate(net = replace_na(net, 0), dom = replace_na(dom, "Mineral-mediated"))

# ---- Scale net to [-1, 1] for fill ----
rng <- max(1e-6, max(abs(cell$net), na.rm=TRUE))
cell <- cell %>% mutate(net_s = pmax(-rng, pmin(rng, net)) / rng)

# ---- Glyphs: vertical arrows ----
cell$glyph <- ifelse(cell$net_s >  0.05, "\u2191", ifelse(cell$net_s < -0.05, "\u2193", "\u2022"))

# ---- y ordering (top-to-bottom) ----
cell$element <- factor(cell$element, levels = rev(ELEMENTS))

# ---- Plot function for a single DOC page ----
make_page <- function(doc_label, out_stub) {
  df <- cell %>% filter(DOC == doc_label)

  p_core <- ggplot(df, aes(x = pH_band, y = element)) +
    geom_tile(aes(fill = net_s), color = "grey88", linewidth = 0.35) +
    geom_text(aes(label = glyph, color = dom), size = 3.5, show.legend = TRUE) +
    scale_fill_gradient2(
      low = FILL_LOW, mid = FILL_MID, high = FILL_HIGH, limits = c(-1, 1),
      name = "Net redox-driven mobility", breaks = c(-1, 0, 1),
      labels = c("immobilize", "neutral", "mobilize")
    ) +
    scale_color_manual(values = proc_col, name = "Process",
                       guide = guide_legend(override.aes = list(label = "\u2191 \u2022 \u2193", size = 4))) +
    facet_grid(context ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = 9.8) +
    theme(
      text = element_text(color = "black"),
      panel.grid = element_blank(),
      strip.text = element_text(face="bold", color = "black"),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 8.8, lineheight = 0.95, vjust = 1, hjust = 0.5, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(color = "black"),
      legend.text  = element_text(color = "black"),
      plot.title = element_text(face="bold", size=13, color = "black"),
      plot.subtitle = element_blank(),
      plot.margin = margin(12,6,4,6)  # extra top margin for subheading
    ) +
    labs(title = NULL)

  # Add a bold subheading above the facet strip using cowplot
  p <- ggdraw(p_core) +
    draw_label(doc_label, x = 0.02, y = 0.995, hjust = 0, vjust = 1,
               fontface = "bold", size = 10, color = "black")

  ggsave(glue("{out_stub}.tiff"), p, width = FIG_W, height = FIG_H)
  ggsave(glue("{out_stub}.png"), p, width = FIG_W, height = FIG_H, dpi = 600)
}

# ---- Build three pages ----
make_page(doc_labels[1], "concept_heatmap_network_byDOC_LOW_BLACKTEXT_SUB")
make_page(doc_labels[2], "concept_heatmap_network_byDOC_MODERATE_BLACKTEXT_SUB")
make_page(doc_labels[3], "concept_heatmap_network_byDOC_HIGH_BLACKTEXT_SUB")

