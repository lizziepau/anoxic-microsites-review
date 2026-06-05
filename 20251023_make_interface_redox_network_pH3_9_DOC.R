
# ======================================================================
# make_interface_redox_network_pH3_9_DOC.R
# Network of redox interactions for Fe, Mn, As, Sb, Cr, V, Tc, Pu, U, S, Se
# across the interface between Bulk Oxic Soil and an Anoxic Microsite.
# Facets: pH bands (3–5, 5–7, 7–9) × DOC (Low/Moderate/High).
# Outputs: paginated panels + separate legend panel.
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(ggplot2); library(glue); library(scales)
  library(igraph); library(ggraph); library(ggforce); library(cowplot)
})

# ---------------------------- USER I/O ---------------------------------
NODES_CSV <- "redox_network_nodes.csv"
EDGES_CSV <- "redox_network_edges_DOC.csv"

OUT_STUB  <- "interface_redox_network_pH3_9_DOC"
FIG_W <- 7; FIG_H <- 4.167          # journal-friendly (fits 4.167in x 7in if needed)
PANELS_PER_PAGE <- 6                # reduce to avoid crowding (2 rows × 3 cols)
WRAP_NCOL <- 3                      # columns per page

# --------------------------- ELEMENT LIST ------------------------------
ELEMENTS <- c("Fe","Mn","As","Sb","Cr","V","Tc","Pu","U","S","Se")

# --------------------------- READ DATA ---------------------------------
stopifnot(file.exists(NODES_CSV), file.exists(EDGES_CSV))
nodes_raw <- suppressMessages(readr::read_csv(NODES_CSV, show_col_types = FALSE))
edges_raw <- suppressMessages(readr::read_csv(EDGES_CSV, show_col_types = FALSE))

# --------------------------- NORMALIZATION -----------------------------
# Expect edges to carry: from, to, mediator, mobility_effect, context (Bulk oxic soil / Anoxic microsite),
# DOC (Low DOC (<2 mg C/L) / Moderate DOC (2–10 mg C/L) / High DOC (>10 mg C/L)),
# pH_band or numeric pH; doc_weight (optional), label (optional)
edges <- edges_raw %>%
  mutate(
    DOC = case_when(
      str_detect(DOC, regex("low", ignore_case=TRUE)) ~ "Low DOC (<2 mg C/L)",
      str_detect(DOC, regex("moderate|mid", ignore_case=TRUE)) ~ "Moderate DOC (2–10 mg C/L)",
      str_detect(DOC, regex("high", ignore_case=TRUE)) ~ "High DOC (>10 mg C/L)",
      TRUE ~ DOC
    ),
    context = case_when(
      str_detect(context, regex("bulk", ignore_case=TRUE)) ~ "Bulk oxic soil",
      str_detect(context, regex("anox", ignore_case=TRUE)) ~ "Anoxic microsite",
      TRUE ~ context
    )
  )

# Bin pH (supports either 'pH' numeric or 'pH_band' text)
if ("pH" %in% names(edges)) {
  edges <- edges %>% mutate(pH_band = cut(pH,
                                          breaks = c(3,5,7,9),
                                          include.lowest = TRUE,
                                          right = FALSE,
                                          labels = c("Acidic (3–5)","Circumneutral (5–7)","Alkaline (7–9)")))
} else if ("pH_band" %in% names(edges)) {
  # map any variants to the 3 bands
  edges <- edges %>% mutate(pH_band = case_when(
    str_detect(pH_band, "3") | str_detect(pH_band, "4") | str_detect(pH_band, "5[^\\d]") ~ "Acidic (3–5)",
    str_detect(pH_band, "6") | str_detect(pH_band, "5–7") | str_detect(pH_band, "6.0–7.5") ~ "Circumneutral (5–7)",
    TRUE ~ "Alkaline (7–9)"
  ))
} else {
  stop("Edges must include either 'pH' numeric or 'pH_band' character.")
}

edges$pH_band <- factor(edges$pH_band, levels = c("Acidic (3–5)","Circumneutral (5–7)","Alkaline (7–9)"))
edges$DOC <- factor(edges$DOC, levels = c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)"))
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- suppressWarnings(as.numeric(edges$doc_weight))

# mediator → process
edges$process <- case_when(
  str_detect(mediator %||% "", regex("microb", TRUE)) ~ "Microbial",
  str_detect(mediator %||% "", regex("oxide|mineral|Fe\\(III\\)|Mn\\(IV\\)|sorb", TRUE)) ~ "Mineral-mediated",
  TRUE ~ "Abiotic"
)

# mobility score
edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- case_when(
  mobility_effect == "mobilize" ~ +1 * edges$w,
  mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

# Node normalization: expect nodes to carry 'name' and 'element'
nodes <- nodes_raw %>%
  transmute(name = name, element = if_else(is.na(element) | element=="", "default", element)) %>%
  mutate(element = ifelse(element %in% c("Vanadium","V(V)"), "V", element))

# --------------------------- MAP ELEMENTS ------------------------------
norm_key <- function(x) {
  x %>% str_replace_all("₀","0") %>% str_replace_all("₁","1") %>% str_replace_all("₂","2") %>%
  str_replace_all("₃","3") %>% str_replace_all("₄","4") %>% str_replace_all("₅","5") %>%
  str_replace_all("₆","6") %>% str_replace_all("₇","7") %>% str_replace_all("₈","8") %>%
  str_replace_all("₉","9") %>% str_squish() %>% tolower()
}
nodes$key <- norm_key(nodes$name)
edges$from_key <- norm_key(edges$from); edges$to_key <- norm_key(edges$to)
map_el <- nodes %>% select(key, element) %>% mutate(element = ifelse(is.na(element),"default",element))

edges <- edges %>%
  left_join(map_el, by = c("from_key"="key")) %>% rename(from_el = element) %>%
  left_join(map_el, by = c("to_key"="key"))   %>% rename(to_el   = element)

edges$element <- ifelse(edges$from_el %in% ELEMENTS, edges$from_el,
                        ifelse(edges$to_el %in% ELEMENTS, edges$to_el, "default"))
edges <- edges %>% filter(element %in% ELEMENTS)

# --------------------------- LAYOUT -----------------------------------
# Manual two-lane layout: x=0 (Bulk oxic soil), x=1 (Anoxic microsite).
elem_levels <- rev(ELEMENTS)
lane_df <- tibble(
  element = rep(elem_levels, times = 2),
  context = rep(c("Bulk oxic soil","Anoxic microsite"), each = length(elem_levels)),
  x = rep(c(0, 1), each = length(elem_levels)),
  y = rep(seq_along(elem_levels), times = 2)
)

# Assemble edges with coordinates
edges_plot <- edges %>%
  select(from_key, to_key, element, context, DOC, pH_band, process, mobility_effect, score, w) %>%
  left_join(nodes %>% select(key, name), by = c("from_key"="key")) %>% rename(from_name = name) %>%
  left_join(nodes %>% select(key, name), by = c("to_key"="key"))   %>% rename(to_name   = name) %>%
  left_join(lane_df %>% rename(context_from = context, x_from = x, y_from = y),
            by = c("element","context"="context_from")) %>%
  left_join(lane_df %>% rename(context_to = context, x_to = x, y_to = y),
            by = c("element","context"="context_to")) %>%
  mutate(
    x_to = ifelse(is.na(x_to), ifelse(context=="Bulk oxic soil", 1, 0), x_to),
    y_to = ifelse(is.na(y_to), y_from, y_to)
  )

nodes_plot <- lane_df %>% mutate(label = element)

# Edge aesthetics
col_mob <- c(mobilize = "#B33C1A", neutral = "#888888", immobilize = "#2F4A9C")
lty_proc <- c(Abiotic = "solid", `Mineral-mediated` = "dashed", Microbial = "dotted")

# DOC weight to width
w_rng <- max(1e-6, max(abs(edges_plot$w), na.rm=TRUE))
edges_plot$w_scaled <- 0.3 + 1.2 * (pmin(edges_plot$w, w_rng) / w_rng)

# --------------------------- BASE PLOT ---------------------------------
base_plot <- ggplot() +
  geom_curve(
    data = edges_plot,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
        color = mobility_effect, linetype = process, size = w_scaled),
    curvature = 0.15, alpha = 0.55, show.legend = TRUE
  ) +
  scale_size_identity(guide = "none") +
  scale_color_manual(values = col_mob, name = "Effect on mobility",
                     breaks = c("mobilize","neutral","immobilize"),
                     labels = c("mobilize","neutral","immobilize")) +
  scale_linetype_manual(values = lty_proc, name = "Mediator/process") +
  # nodes
  geom_point(data = nodes_plot, aes(x = x, y = y), shape = 21, size = 2.9,
             fill = "white", color = "black", stroke = 0.35) +
  geom_text(data = nodes_plot, aes(x = x, y = y, label = label),
            size = 2.8, vjust = -1.1, fontface = "bold") +
  scale_x_continuous(breaks = c(0,1), labels = c("Bulk oxic soil","Anoxic microsite"), limits = c(-0.2,1.2)) +
  scale_y_continuous(breaks = seq_along(elem_levels), labels = elem_levels,
                     expand = expansion(add = c(0.3, 0.3))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 9.8) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8.5),
    legend.title = element_text(size = 9.5),
    plot.margin = margin(6,6,6,6)
  ) +
  labs(title = "Redox interaction network across the oxic–anoxic interface")

# --------------------------- FACETING / PAGINATION ---------------------
edges_plot$facet_key <- interaction(edges_plot$DOC, edges_plot$pH_band, sep = " • ")

plot_page <- function(page = 1) {
  p <- base_plot +
    ggforce::facet_wrap_paginate(~ facet_key, ncol = WRAP_NCOL,
                                 nrow = ceiling(PANELS_PER_PAGE / WRAP_NCOL),
                                 page = page, scales = "fixed") +
    guides(
      color = guide_legend(order = 1, title.position = "top"),
      linetype = guide_legend(order = 2, title.position = "top")
    )
  p
}

p1 <- plot_page(1)
pages <- ggforce::n_pages(p1)

for (pg in seq_len(pages)) {
  g <- plot_page(pg)
  ggsave(glue("{OUT_STUB}_page{sprintf('%02d', pg)}.png"), g, width = FIG_W, height = FIG_H, dpi = 600)
  ggsave(glue("{OUT_STUB}_page{sprintf('%02d', pg)}.svg"), g, width = FIG_W, height = FIG_H)
}

# --------------------------- LEGEND PANEL -------------------------------
leg <- cowplot::get_legend(
  base_plot +
    guides(
      color = guide_legend(title.position = "top"),
      linetype = guide_legend(title.position = "top")
    ) +
    theme(legend.position = "bottom")
)
leg_plot <- cowplot::ggdraw(leg)
ggsave(glue("{OUT_STUB}_LEGEND.png"), leg_plot, width = 6, height = 1.6, dpi = 600)
ggsave(glue("{OUT_STUB}_LEGEND.svg"), leg_plot, width = 6, height = 1.6)
