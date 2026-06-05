
# ===================================================================
# CONCEPTUAL / GLYPH MODEL OF REACTION NETWORK
# See comments in previous message for details.
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales)
})

nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7; FIG_H <- 4.167
ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V")
PANEL_TITLE <- "Reaction network in oxic soil with anoxic microsites — conceptual glyph view"
SUBTITLE_TPL <- "{doc} • Columns = pH (numeric ranges); rows = context • Arrow color = effect on mobility; width = magnitude"

col_mob <- c(mobilize="#3b5b92", immobilize="#a35e10", neutral="#7a7a7a")
stroke_map <- c(Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
                Tc="#D55E00", Pu="#F0E442", U="#999999", Se="#33CC33", V="#AA4499", default="#666666")
proc_types <- c(Abiotic=1, `Mineral-mediated`=2, Microbial=3)

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

edges <- edges %>% mutate(
  process = case_when(
    mediator %in% c("abiotic","O₂","O2") ~ "Abiotic",
    mediator %in% c("Mn(IV) oxides","Mn/Fe oxides","O₂/Mn oxides","carbonate","O₂/pH","O₂/NO₃⁻","carbonate/OM/O₂") ~ "Mineral-mediated",
    mediator %in% c("microbial","microbial/OM","microbial/Fe(II)","microbial/Fe(II)/sulfide","Fe(II)/sulfide/bioreduction") ~ "Microbial",
    mediator %in% c("abiotic/biotic","Fe(II)/sulfide/OM","Fe(II)/sulfide") ~ "Mineral-mediated",
    TRUE ~ "Mineral-mediated"
  )
)

edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- case_when(
  edges$mobility_effect == "mobilize"   ~ +1 * edges$w,
  edges$mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

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
edges <- edges %>% mutate(
  element = case_when(
    from_el %in% ELEMENTS ~ from_el,
    to_el   %in% ELEMENTS ~ to_el,
    TRUE ~ "default"
  )
)
edges_filt <- edges %>% filter(element %in% ELEMENTS)

cell <- edges_filt %>%
  mutate(proc_weight = 1) %>%
  group_by(DOC, pH_band, context, element) %>%
  summarise(
    net = sum(score, na.rm=TRUE),
    n   = n(),
    dom = names(which.max(c(
            Abiotic = sum(process=="Abiotic"),
            `Mineral-mediated` = sum(process=="Mineral-mediated"),
            Microbial = sum(process=="Microbial")
          ))),
    .groups = "drop"
  ) %>%
  mutate(
    effect = case_when(
      net >  0.05 ~ "mobilize",
      net < -0.05 ~ "immobilize",
      TRUE ~ "neutral"
    ),
    mag = scales::rescale(abs(net), to=c(0.6, 2.0))
  )

all_cells <- expand.grid(
  DOC = levels(edges$DOC),
  pH_band = levels(edges$pH_band),
  context = levels(edges$context),
  element = ELEMENTS
) %>% as_tibble()
cell <- all_cells %>% left_join(cell, by=c("DOC","pH_band","context","element")) %>%
  mutate(
    effect = replace_na(effect, "neutral"),
    dom    = replace_na(dom, "Mineral-mediated"),
    mag    = replace_na(mag, 0.6),
    net    = replace_na(net, 0)
  )

proc_types <- c(Abiotic=1, `Mineral-mediated`=2, Microbial=3)
cell$linetype <- recode(cell$dom, !!!setNames(as.list(proc_types), names(proc_types)))

el_levels <- rev(ELEMENTS)
cell$el_y <- match(cell$element, el_levels)
x0 <- 0.28; x1 <- 0.78
cell <- cell %>% mutate(
  x_start = ifelse(effect=="immobilize", x1, x0),
  x_end   = ifelse(effect=="immobilize", x0, x1),
  y = el_y
)

make_page <- function(doc_label) {
  df <- cell %>% filter(DOC == doc_label)

  p <- ggplot(df) +
    geom_segment(aes(x=x_start, y=y, xend=x_end, yend=y, color=effect, linewidth=mag, linetype=dom),
                 lineend="round", show.legend = FALSE,
                 arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +
    geom_text(data = distinct(df, context, element, y),
              aes(x = 0.02, y = y, label = element, color = element),
              hjust = 0, size = 3.6, show.legend = FALSE) +
    scale_color_manual(values = c(col_mob, stroke_map), guide="none") +
    scale_linewidth(range = c(0.6, 2.2)) +
    scale_linetype_manual(values = proc_types) +
    scale_y_continuous(breaks = seq_along(el_levels), labels = el_levels, expand = expansion(add = c(0.6,0.6))) +
    scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
    facet_grid(context ~ pH_band) +
    theme_minimal(base_size = 9.5) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face="bold"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face="bold", size=13),
      plot.subtitle = element_text(size=10),
      plot.margin = margin(4,6,4,6)
    ) +
    labs(
      title = PANEL_TITLE,
      subtitle = glue(SUBTITLE_TPL, doc = doc_label)
    )

  ggsave(sprintf("concept_glyph_network_%s.svg", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H)
  ggsave(sprintf("concept_glyph_network_%s.png", gsub(' ','_',tolower(doc_label))), p, width=FIG_W, height=FIG_H, dpi=600)
}

invisible(lapply(levels(edges$DOC), make_page))
