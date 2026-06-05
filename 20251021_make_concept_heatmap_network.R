
# ===================================================================
# CONCEPTUAL HEATMAP MODEL OF REACTION NETWORK
# - Input: redox_network_nodes.csv, redox_network_edges_DOC.csv
# - Output: 3 pages (Low/Moderate/High DOC), each 7 x 4.167 in
#   Columns = pH bands (with numeric ranges)
#   Rows    = context (Bulk oxic soil / Anoxic microsite)
#   Y-axis  = element symbols
#   Tile fill color = net effect on mobility (immobilize ← 0 → mobilize)
#   Tile text       = arrow (→ mobilize, ← immobilize, • neutral)
#   Tile border     = dominant process class (solid / dashed / dotted)
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales)
})

nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7; FIG_H <- 4.167
ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V")
PANEL_TITLE <- "Reaction network in oxic soil with anoxic microsites — heatmap conceptual view"
SUBTITLE_TPL <- "{doc} • Columns = pH (numeric ranges); rows = context • Fill = net mobility; border = process"

# palettes
col_div <- scales::div_gradient_pal(low = "tomato", mid = "#cfcfcf", high = "blue")  # brown→grey→blue
proc_lty <- c(Abiotic=1, `Mineral-mediated`=2, Microbial=3)

# factor normalization
edges$pH_band <- factor(edges$pH_band,
  levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"),
  labels = c("Acidic (pH 4.5–6.0)","Circumneutral (pH 6.0–7.5)","Alkaline (pH 7.5–8.5)")
)
edges$DOC <- factor(edges$DOC,
  levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"),
  labels = c("Low DOC","Moderate DOC","High DOC")
)
edges$context <- factor(edges$context, levels=c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- as.numeric(edges$doc_weight)

nodes$element <- ifelse(is.na(nodes$element)|nodes$element=="","default",nodes$element)
nodes$element[nodes$element %in% c("Vanadium","V(V)")] <- "V"

# process class mapping
edges <- edges %>% mutate(
  process = case_when(
    mediator %in% c("abiotic","O₂","O2") ~ "Abiotic",
    mediator %in% c("Mn(IV) oxides","Mn/Fe oxides","O₂/Mn oxides","carbonate","O₂/pH","O₂/NO₃⁻","carbonate/OM/O₂") ~ "Mineral-mediated",
    mediator %in% c("microbial","microbial/OM","microbial/Fe(II)","microbial/Fe(II)/sulfide","Fe(II)/sulfide/bioreduction") ~ "Microbial",
    mediator %in% c("abiotic/biotic","Fe(II)/sulfide/OM","Fe(II)/sulfide") ~ "Mineral-mediated",
    TRUE ~ "Mineral-mediated"
  )
)

# net mobility score (+1 mobilize, -1 immobilize, 0 neutral) * doc_weight
edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- case_when(
  edges$mobility_effect == "mobilize"   ~ +1 * edges$w,
  edges$mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

# map edge names to elements
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

# aggregate to cells
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

# ensure rectangular grid
all_cells <- expand.grid(
  DOC = levels(edges$DOC),
  pH_band = levels(edges$pH_band),
  context = levels(edges$context),
  element = ELEMENTS
) %>% as_tibble()

cell <- all_cells %>% left_join(cell, by=c("DOC","pH_band","context","element")) %>%
  mutate(net = replace_na(net, 0), dom = replace_na(dom, "Mineral-mediated"))

# cap + scale net to [-1,1] for color
rng <- max(1e-6, max(abs(cell$net), na.rm=TRUE))
cell <- cell %>% mutate(net_s = pmax(-rng, pmin(rng, net)) / rng)

# arrow glyph inside tile
cell$arrow <- ifelse(cell$net_s >  0.05, "\u2192", ifelse(cell$net_s < -0.05, "\u2190", "\u2022"))

# y ordering (top-to-bottom descending)
cell$element <- factor(cell$element, levels = rev(ELEMENTS))

make_page <- function(doc_label) {
  df <- cell %>% filter(DOC == doc_label)

  p <- ggplot(df, aes(x = pH_band, y = element)) +
    geom_tile(aes(fill = net_s), color = "grey85", linewidth = 0.3) +
    geom_text(aes(label = arrow), size = 3.3, color = "black") +
    scale_fill_gradient2(low = "tomato", mid = "#e8e8e8", high = "blue",
                         limits = c(-1, 1), name = "Net mobility",
                         breaks = c(-1, 0, 1), labels = c("immobilize", "neutral", "mobilize")) +
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

  ggsave(sprintf("concept_heatmap_network_%s.svg", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H)
  ggsave(sprintf("concept_heatmap_network_%s.png", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H, dpi=600)
}

invisible(lapply(levels(edges$DOC), make_page))

