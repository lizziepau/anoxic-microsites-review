
# ===================================================================
# THREE PANELS (no legend) + SEPARATE LEGEND PANEL
# National Park Colors edition (Yellowstone for heatmap; RockyMountains for Process)
# File: make_concept_heatmap_network_S_VERTICAL_byDOC_NPCOLORS_NOLEG_AND_LEGEND.R
# ===================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(stringr); library(ggplot2); library(glue); library(scales); library(cowplot)
  library(nationalparkcolors)
})

nodes <- readr::read_csv("redox_network_nodes.csv", show_col_types = FALSE)
edges <- readr::read_csv("redox_network_edges_DOC.csv", show_col_types = FALSE)

FIG_W <- 7; FIG_H <- 4.167
LEG_W <- 6.5; LEG_H <- 2.0

ELEMENTS <- c("Fe","Mn","As","Cr","U","Pu","Tc","Se","Sb","V","S")
PANEL_TITLE <- "Reaction network in bulk oxic soil with anoxic microsites"

FILL_PAL <- park_palette("Yellowstone", n = 100, type = "continuous")
PROC_PAL <- park_palette("RockyMountains", n = 3, type = "discrete")
names(PROC_PAL) <- c("Abiotic", "Mineral-mediated", "Microbial")

nbsp_thin <- "\u202F"

edges$pH_band <- factor(edges$pH_band,
  levels = c("acidic (4.5–6.0)","circumneutral (6.0–7.5)","alkaline (7.5–8.5)"),
  labels = c("Acidic\n(pH 4.5–6.0)", "Circumneutral\n(pH 6.0–7.5)", "Alkaline\n(pH 7.5–8.5)")
)
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
edges$score <- dplyr::case_when(
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
edges$from_key <- norm_key(edges$from); edges$to_key <- norm_key(edges$to)
map_el <- nodes %>% select(key, element) %>% mutate(element = ifelse(is.na(element),"default",element))
edges <- edges %>%
  left_join(map_el, by=c("from_key"="key")) %>% rename(from_el = element) %>%
  left_join(map_el, by=c("to_key"="key"))   %>% rename(to_el   = element)
edges$element <- ifelse(edges$from_el %in% ELEMENTS, edges$from_el,
                        ifelse(edges$to_el %in% ELEMENTS, edges$to_el, "default"))
edges_filt <- edges %>% filter(element %in% ELEMENTS)

cell <- edges_filt %>%
  group_by(DOC, pH_band, context, element) %>%
  summarise(
    net = sum(score, na.rm=TRUE),
    dom = names(which.max(c(Abiotic = sum(process=="Abiotic"),
                            `Mineral-mediated` = sum(process=="Mineral-mediated"),
                            Microbial = sum(process=="Microbial")))),
    .groups = "drop"
  )

all_cells <- expand.grid(
  DOC = levels(edges$DOC),
  pH_band = levels(edges$pH_band),
  context = levels(edges$context),
  element = ELEMENTS
) %>% as_tibble()
cell <- all_cells %>% left_join(cell, by=c("DOC","pH_band","context","element")) %>%
  mutate(net = replace_na(net, 0), dom = replace_na(dom, "Mineral-mediated"))

rng <- max(1e-6, max(abs(cell$net), na.rm=TRUE))
cell <- cell %>% mutate(net_s = pmax(-rng, pmin(rng, net)) / rng)
cell$element <- factor(cell$element, levels = rev(ELEMENTS))

build_plot <- function(df, show_legend = FALSE) {
  ggplot(df, aes(x = pH_band, y = element)) +
    geom_tile(aes(fill = net_s), color = "grey88", linewidth = 0.35) +
    geom_point(shape = 16, size = 3.4, color = "black", show.legend = FALSE) +
    geom_point(aes(color = dom), shape = 16, size = 2.8, show.legend = show_legend) +
    scale_fill_gradientn(
      colours = FILL_PAL,
      limits = c(-1, 1),
      name = "Net redox-driven mobility",
      breaks = c(-1, 0, 1),
      labels = c("immobilize", "neutral", "mobilize")
    ) +
    scale_color_manual(values = PROC_PAL, name = "Process") +
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
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(color = "black"),
      legend.text  = element_text(color = "black"),
      plot.title = element_text(face="bold", size=13, color = "black"),
      plot.subtitle = element_blank(),
      plot.margin = margin(12,6,4,6)
    ) +
    labs(title = PANEL_TITLE)
}

make_page <- function(doc_label, out_stub) {
  df <- cell %>% filter(DOC == doc_label)
  p_core <- build_plot(df, show_legend = FALSE)
  p <- cowplot::ggdraw(p_core) +
    cowplot::draw_label(doc_label, x = 0.02, y = 0.995, hjust = 0, vjust = 1,
                        fontface = "bold", size = 10, color = "black")
  ggsave(glue("{out_stub}.svg"), p, width = FIG_W, height = FIG_H)
  ggsave(glue("{out_stub}.png"), p, width = FIG_W, height = FIG_H, dpi = 600)
}

save_legend_panel <- function(out_stub = "concept_heatmap_network_LEGEND_PANEL_NPCOLORS") {
  # Synthetic Process legend data (ensures all categories appear)
  proc_levels <- names(PROC_PAL)
  df_proc <- tibble(
    x = seq_along(proc_levels),
    y = 1,
    dom = factor(proc_levels, levels = proc_levels)
  )
  p_proc <- ggplot(df_proc, aes(x, y)) +
    geom_point(shape = 21, aes(fill = dom), color = "black", size = 3, show.legend = TRUE) +
    scale_fill_manual(values = PROC_PAL, name = "Process") +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black"))) +
    theme_void(base_size = 10) +
    theme(legend.position = "right")
  leg_proc <- cowplot::get_legend(p_proc)

  # Synthetic Mobility legend data
  df_fill <- tibble(
    x = 1,
    y = seq_len(50),
    net_s = seq(-1, 1, length.out = 50)
  )
  p_fill <- ggplot(df_fill, aes(x, y)) +
    geom_raster(aes(fill = net_s)) +
    scale_fill_gradientn(
      colours = FILL_PAL,
      limits = c(-1, 1),
      name = "Net redox-driven mobility",
      breaks = c(-1, 0, 1),
      labels = c("immobilize", "neutral", "mobilize")
    ) +
    guides(fill = guide_colorbar(
      barwidth = unit(55, "mm"),
      barheight = unit(4, "mm"),
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom"
    )) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom")
  leg_fill <- cowplot::get_legend(p_fill)

  leg_plot <- cowplot::plot_grid(leg_fill, leg_proc, nrow = 1, rel_widths = c(1.4, 0.8), align = "h")
  ggsave(glue("{out_stub}.svg"), leg_plot, width = LEG_W, height = LEG_H)
  ggsave(glue("{out_stub}.png"), leg_plot, width = LEG_W, height = LEG_H, dpi = 600)
}

make_page(doc_labels[1], "concept_heatmap_network_byDOC_LOW_NPCOLORS_SUB_NOLEG")
make_page(doc_labels[2], "concept_heatmap_network_byDOC_MODERATE_NPCOLORS_SUB_NOLEG")
make_page(doc_labels[3], "concept_heatmap_network_byDOC_HIGH_NPCOLORS_SUB_NOLEG")
save_legend_panel("concept_heatmap_network_LEGEND_PANEL_NPCOLORS")
