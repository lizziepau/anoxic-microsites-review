
# ===================================================================
# CONCEPTUAL HEATMAP MODEL — with Sulfur + VERTICAL arrows
# Arrow color encodes dominant process (Abiotic / Mineral-mediated / Microbial)
# Tile linetype removed for a simpler legend.
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales)
})

nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7; FIG_H <- 4.167
ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V","S")  # include Sulfur
PANEL_TITLE <- "Reaction network in oxic soil with anoxic microsites — heatmap conceptual view"
SUBTITLE_TPL <- "{doc} • Columns = pH (numeric ranges); rows = context • Fill = net mobility; arrow color = process"

# Tile fill gradient (immobilize ← 0 → mobilize)
FILL_LOW  <- "#b57434"; FILL_MID  <- "#e8e8e8"; FILL_HIGH <- "#3b5b92"

# Process colors for arrows
proc_col  <- c(Abiotic="#2E8B57", `Mineral-mediated`="#E69F00", Microbial="#800080")

# ---- Factor normalization ----
edges$pH_band <- factor(edges$pH_band,
  levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"),
  labels = c("Acidic (pH 4.5–6.0)","Circumneutral (pH 6.0–7.5)","Alkaline (pH 7.5–8.5)")
)
edges$DOC <- factor(edges$DOC,
  levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"),
  labels = c("Low DOC","Moderate DOC","High DOC")
)
edges$context <- factor(edges$context, levels=c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- suppressWarnings(as.numeric(edges$doc_weight))

# Normalize node elements (collapse "Vanadium"/"V(V)" -> "V")
nodes$element <- ifelse(is.na(nodes$element)|nodes$element=="","default",nodes$element)
nodes$element[nodes$element %in% c("Vanadium","V(V)")] <- "V"

# ---- Map mediator to 3 process classes ----
edges <- edges %>% mutate(
  process = case_when(
    mediator %in% c("abiotic","O₂","O2") ~ "Abiotic",
    mediator %in% c("Mn(IV) oxides","Mn/Fe oxides","O₂/Mn oxides","carbonate","O₂/pH","O₂/NO₃⁻","carbonate/OM/O₂") ~ "Mineral-mediated",
    mediator %in% c("microbial","microbial/OM","microbial/Fe(II)","microbial/Fe(II)/sulfide","Fe(II)/sulfide/bioreduction") ~ "Microbial",
    mediator %in% c("abiotic/biotic","Fe(II)/sulfide/OM","Fe(II)/sulfide") ~ "Mineral-mediated",
    TRUE ~ "Mineral-mediated"
  )
)

# ---- Net mobility score (+1 mobilize, -1 immobilize, 0 neutral) * doc_weight ----
edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- dplyr::case_when(
  edges$mobility_effect == "mobilize"   ~ +1 * edges$w,
  edges$mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

# ---- Map edge names to elements (via node keys) ----
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
edges <- edges %>% mutate(element = ifelse(from_el %in% ELEMENTS, from_el, ifelse(to_el %in% ELEMENTS, to_el, "default")))
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

# Fill in empty cells for a complete grid
all_cells <- expand.grid(
  DOC = levels(edges$DOC),
  pH_band = levels(edges$pH_band),
  context = levels(edges$context),
  element = ELEMENTS
) %>% as_tibble()
cell <- all_cells %>% left_join(cell, by=c("DOC","pH_band","context","element")) %>%
  mutate(net = replace_na(net, 0), dom = replace_na(dom, "Mineral-mediated"))

# Scale net to [-1, 1] for the fill gradient
rng <- max(1e-6, max(abs(cell$net), na.rm=TRUE))
cell <- cell %>% mutate(net_s = pmax(-rng, pmin(rng, net)) / rng)

# VERTICAL symbol per tile
cell$glyph <- ifelse(cell$net_s >  0.05, "\u2191", ifelse(cell$net_s < -0.05, "\u2193", "\u2022"))
cell$element <- factor(cell$element, levels = rev(ELEMENTS))

# ---- Plot function ----
make_page <- function(doc_label) {
  df <- cell %>% filter(DOC == doc_label)

  p <- ggplot(df, aes(x = pH_band, y = element)) +
    geom_tile(aes(fill = net_s), color = "grey88", linewidth = 0.35) +
    geom_text(aes(label = glyph, color = dom), size = 3.5, show.legend = TRUE) +
    scale_fill_gradient2(
      low = FILL_LOW, mid = FILL_MID, high = FILL_HIGH, limits = c(-1, 1),
      name = "Net redox-driven mobility", breaks = c(-1, 0, 1),
      labels = c("immobilize", "neutral", "mobilize")
    ) +
    scale_color_manual(values = proc_col, name = "Process") +
    facet_grid(context ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = 9.5) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face="bold"),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.ticks = element_blank(),
      legend.position = "right",
      plot.title = element_text(face="bold", size=13),
      plot.subtitle = element_text(size=10),
      plot.margin = margin(4,6,4,6)
    ) +
    labs(
      title = PANEL_TITLE,
      subtitle = glue(SUBTITLE_TPL, doc = doc_label)
    )

  ggsave(sprintf("concept_heatmap_network_S_VERTICAL_%s.svg", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H)
  ggsave(sprintf("concept_heatmap_network_S_VERTICAL_%s.png", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H, dpi = 600)
}

invisible(lapply(levels(edges$DOC), make_page))
