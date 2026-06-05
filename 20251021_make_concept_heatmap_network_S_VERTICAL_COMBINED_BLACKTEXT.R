
# ===================================================================
# CONCEPTUAL HEATMAP — COMBINED (All DOC columns), Landscape Wide
# File: make_concept_heatmap_network_S_VERTICAL_COMBINED_BLACKTEXT.R
# - All UI text in black
# - Title only (no subtitle)
# - Process legend shows "↑ • ↓" for each process color
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales)
})

# ---- IO ----
nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

# ---- Figure size ----
FIG_W <- 10.5; FIG_H <- 4.167

# ---- Elements ----
ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V","S")

# ---- Titles ----
PANEL_TITLE <- "Reaction network in bulk oxic soil with anoxic microsites"

# ---- Palettes ----
FILL_LOW  <- "#E1341E"; FILL_MID  <- "#e8e8e8"; FILL_HIGH <- "#1ECBE1"
proc_col  <- c(Abiotic="#961EE1", `Mineral-mediated`="goldenrod", Microbial="seagreen")

# Heatmap (tile fill)
#scale_fill_gradientn(
 # colours = viridis::cividis(256),
  #name = "Net redox-driven mobility",
#  limits = c(-1, 1),
 # breaks = c(-1, 0, 1),
  #labels = c("immobilize", "neutral", "mobilize")
#)

# Arrow/process color mapping
#proc_col <- c(
 # Abiotic = "#961EE1",           # Blue
  #`Mineral-mediated` = "goldenrod", # Gold
#  Microbial = "seagreen"           # Teal
#)

# Thin space (U+202F) for units
nbsp_thin <- "\u202F"

# ---- Factor normalization ----
edges$pH_band <- factor(edges$pH_band,
  levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"),
  labels = c("Acidic\n(pH 4.5–6.0)", "Circumneutral\n(pH 6.0–7.5)", "Alkaline\n(pH 7.5–8.5)")
)

# DOC facet labels with scientific-style units and superscripts
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

# ---- Plot combined figure ----
p <- ggplot(cell, aes(x = pH_band, y = element)) +
  geom_tile(aes(fill = net_s), color = "grey88", linewidth = 0.35) +
  geom_text(aes(label = glyph, color = dom), size = 3.4, show.legend = TRUE) +
  scale_fill_gradient2(
    low = FILL_LOW, mid = FILL_MID, high = FILL_HIGH, limits = c(-1, 1),
    name = "Net redox-driven mobility", breaks = c(-1, 0, 1),
    labels = c("immobilize", "neutral", "mobilize")
  ) +
  # Process legend shows "↑ • ↓" per process entry (geom_text legend keys)
  scale_color_manual(values = proc_col, name = "Process",
                     guide = guide_legend(override.aes = list(label = "\u2191 \u2022 \u2193", size = 4))) +
  facet_grid(context ~ DOC, scales = "free_y", space = "free_y") +
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
    plot.margin = margin(4,6,4,6)
  ) +
  labs(
    title = PANEL_TITLE
  )

# ---- Save ----
ggsave("concept_heatmap_network_S_VERTICAL_COMBINED_BLACKTEXT.svg", p, width = FIG_W, height = FIG_H)
ggsave("concept_heatmap_network_S_VERTICAL_COMBINED_BLACKTEXT.png", p, width = FIG_W, height = FIG_H, dpi = 600)
ggsave("concept_heatmap_network_S_VERTICAL_COMBINED_BLACKTEXT.tiff", p, width = FIG_W, height = FIG_H)

