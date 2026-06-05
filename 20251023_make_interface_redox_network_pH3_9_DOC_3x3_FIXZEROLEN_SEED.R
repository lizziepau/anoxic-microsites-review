
# ======================================================================
# make_interface_redox_network_pH3_9_DOC_3x3_FIXZEROLEN_SEED.R
# 3×3 fixed grid with robust handling of zero-length/NA edges
# and a facet "seed" so facet_grid never fails when edges are empty.
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(ggplot2); library(glue); library(scales)
  library(ggforce); library(cowplot)
})

# ---------------------------- USER I/O ---------------------------------
NODES_CSV <- "redox_network_nodes.csv"
EDGES_CSV <- "redox_network_edges_DOC.csv"

OUT_STUB  <- "interface_redox_network_pH3_9_DOC_3x3"
FIG_W <- 4.167; FIG_H <- 7.0

ELEMENTS <- c("Fe","Mn","As","Sb","Cr","V","Tc","Pu","U","S","Se")

stopifnot(file.exists(NODES_CSV), file.exists(EDGES_CSV))
nodes_raw <- suppressMessages(readr::read_csv(NODES_CSV, show_col_types = FALSE))
edges_raw <- suppressMessages(readr::read_csv(EDGES_CSV, show_col_types = FALSE))

# --------------------------- NORMALIZATION -----------------------------
edges <- edges_raw %>%
  mutate(
    DOC = case_when(
      str_detect(DOC, regex("low", TRUE)) ~ "Low DOC (<2 mg C/L)",
      str_detect(DOC, regex("moderate|mid", TRUE)) ~ "Moderate DOC (2–10 mg C/L)",
      str_detect(DOC, regex("high", TRUE)) ~ "High DOC (>10 mg C/L)",
      TRUE ~ DOC
    ),
    context = case_when(
      str_detect(context, regex("bulk", TRUE)) ~ "Bulk oxic soil",
      str_detect(context, regex("anox", TRUE)) ~ "Anoxic microsite",
      TRUE ~ context
    )
  )

# Bin pH to 3–5, 5–7, 7–9
if ("pH" %in% names(edges)) {
  edges <- edges %>% mutate(pH_band = cut(pH,
    breaks = c(3,5,7,9), include.lowest = TRUE, right = FALSE,
    labels = c("Acidic (3–5)","Circumneutral (5–7)","Alkaline (7–9)")))
} else if ("pH_band" %in% names(edges)) {
  edges <- edges %>% mutate(pH_band = case_when(
    str_detect(pH_band, "3") | str_detect(pH_band, "4") | str_detect(pH_band, "3–5") ~ "Acidic (3–5)",
    str_detect(pH_band, "5–7") | str_detect(pH_band, "6") ~ "Circumneutral (5–7)",
    TRUE ~ "Alkaline (7–9)"
  ))
} else stop("Edges must include either 'pH' numeric or 'pH_band' character.")

doc_levels <- c("Low DOC (<2 mg C/L)","Moderate DOC (2–10 mg C/L)","High DOC (>10 mg C/L)")
ph_levels  <- c("Acidic (3–5)","Circumneutral (5–7)","Alkaline (7–9)")

edges$pH_band <- factor(edges$pH_band, levels = ph_levels)
edges$DOC     <- factor(edges$DOC,     levels = doc_levels)
edges$context <- factor(edges$context, levels = c("Bulk oxic soil","Anoxic microsite"))
edges$doc_weight <- suppressWarnings(as.numeric(edges$doc_weight))

# mediator → process
edges$process <- case_when(
  str_detect(edges$mediator %||% "", regex("microb", TRUE)) ~ "Microbial",
  str_detect(edges$mediator %||% "", regex("oxide|mineral|Fe\\(III\\)|Mn\\(IV\\)|sorb", TRUE)) ~ "Mineral-mediated",
  TRUE ~ "Abiotic"
)

# mobility score and width
edges$w <- ifelse(is.na(edges$doc_weight), 1, edges$doc_weight)
edges$score <- case_when(
  edges$mobility_effect == "mobilize"   ~ +1 * edges$w,
  edges$mobility_effect == "immobilize" ~ -1 * edges$w,
  TRUE ~ 0
)

# --------------------------- MAP ELEMENTS ------------------------------
nodes <- nodes_raw %>%
  transmute(name = name, element = if_else(is.na(element) | element=="", "default", element)) %>%
  mutate(element = ifelse(element %in% c("Vanadium","V(V)"), "V", element))

norm_key <- function(x) {
  x %>% stringr::str_replace_all("₀","0") %>% stringr::str_replace_all("₁","1") %>% stringr::str_replace_all("₂","2") %>%
  stringr::str_replace_all("₃","3") %>% stringr::str_replace_all("₄","4") %>% stringr::str_replace_all("₅","5") %>%
  stringr::str_replace_all("₆","6") %>% stringr::str_replace_all("₇","7") %>% stringr::str_replace_all("₈","8") %>%
  stringr::str_replace_all("₉","9") %>% stringr::str_squish() %>% tolower()
}
nodes$key <- norm_key(nodes$name)
edges$from_key <- norm_key(edges$from); edges$to_key <- norm_key(edges$to)
map_el <- nodes %>% select(key, element) %>% mutate(element = ifelse(is.na(element),"default",element))

edges <- edges %>%
  left_join(map_el, by = c("from_key"="key")) %>% rename(from_el = element) %>%
  left_join(map_el, by = c("to_key"="key"))   %>% rename(to_el   = element)

ELEMENTS <- unique(ELEMENTS)
edges$element <- ifelse(edges$from_el %in% ELEMENTS, edges$from_el,
                        ifelse(edges$to_el %in% ELEMENTS, edges$to_el, "default"))
edges <- edges %>% filter(element %in% ELEMENTS)

# --------------------------- LAYOUT -----------------------------------
elem_levels <- rev(ELEMENTS)
lane_df <- tibble::tibble(
  element = rep(elem_levels, times = 2),
  context = rep(c("Bulk oxic soil","Anoxic microsite"), each = length(elem_levels)),
  x = rep(c(0, 1), each = length(elem_levels)),
  y = rep(seq_along(elem_levels), times = 2)
)

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

# ---- FIX 1: drop NA endpoints & zero-length curves ----
edges_plot <- edges_plot %>%
  filter(!is.na(x_from), !is.na(y_from), !is.na(x_to), !is.na(y_to)) %>%
  mutate(is_zerolen = (x_from == x_to & y_from == y_to))

n_drop <- sum(edges_plot$is_zerolen, na.rm = TRUE)
if (n_drop > 0) message(glue("Dropping {n_drop} zero-length edges (from/to identical)."))
edges_plot <- edges_plot %>% filter(!is_zerolen)

nodes_plot <- lane_df %>% mutate(label = element)

# Aesthetics
col_mob <- c(mobilize = "#B33C1A", neutral = "#888888", immobilize = "#2F4A9C")
lty_proc <- c(Abiotic = "solid", `Mineral-mediated` = "dashed", Microbial = "dotted")

w_rng <- max(1e-6, max(abs(edges_plot$w), na.rm=TRUE))
edges_plot$w_scaled <- 0.3 + 1.2 * (pmin(edges_plot$w, w_rng) / w_rng)

# ---- FIX 2: facet seed so facet_grid always has values ----
facet_seed <- tidyr::expand_grid(DOC = factor(doc_levels, levels = doc_levels),
                                 pH_band = factor(ph_levels, levels = ph_levels))

# --------------------------- BASE PLOT ---------------------------------
base_plot <- ggplot() +
  # seed carries facet variables
  geom_blank(data = facet_seed, aes(x = 0, y = 0)) +
  # edges
  { if (nrow(edges_plot) > 0) geom_curve(
      data = edges_plot,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
          color = mobility_effect, linetype = process, size = w_scaled),
      curvature = 0.15, alpha = 0.55, show.legend = TRUE
    ) else NULL } +
  scale_size_identity(guide = "none") +
  scale_color_manual(values = col_mob, name = "Effect on mobility",
                     breaks = c("mobilize","neutral","immobilize"),
                     labels = c("mobilize","neutral","immobilize")) +
  scale_linetype_manual(values = lty_proc, name = "Mediator/process") +
  # nodes (drawn in every facet)
  geom_point(data = nodes_plot, aes(x = x, y = y), shape = 21, size = 2.6,
             fill = "white", color = "black", stroke = 0.35) +
  geom_text(data = nodes_plot, aes(x = x, y = y, label = label),
            size = 2.6, vjust = -1.05, fontface = "bold") +
  scale_x_continuous(breaks = c(0,1), labels = c("Bulk oxic soil","Anoxic microsite"),
                     limits = c(-0.15,1.15), expand = expansion(add = 0.02)) +
  scale_y_continuous(breaks = seq_along(elem_levels), labels = elem_levels,
                     expand = expansion(add = c(0.25, 0.35))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 9.5) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8.8),
    axis.text.y = element_text(size = 8.2),
    strip.text.x = element_text(face = "bold", size = 8.8),
    strip.text.y = element_text(face = "bold", size = 8.8),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8.0),
    legend.title = element_text(size = 8.5),
    plot.title = element_text(face = "bold", size = 10.5),
    plot.margin = margin(4,4,4,4)
  ) +
  labs(title = "Redox interaction network across the oxic–anoxic interface")

# --------------------------- 3×3 FACET GRID ----------------------------
p_3x3 <- base_plot +
  facet_grid(rows = vars(pH_band), cols = vars(DOC), scales = "fixed") +
  guides(
    color = guide_legend(order = 1, title.position = "top"),
    linetype = guide_legend(order = 2, title.position = "top")
  )

# --------------------------- SAVE --------------------------------------
# --------------------------- SAVE --------------------------------------
ggplot2::ggsave(glue("{OUT_STUB}.png"), p_3x3, width = FIG_W, height = FIG_H, dpi = 600)
ggplot2::ggsave(glue("{OUT_STUB}.svg"), p_3x3, width = FIG_W, height = FIG_H)

# Legend panel
leg <- cowplot::get_legend(
  base_plot +
    guides(
      color = guide_legend(title.position = "top"),
      linetype = guide_legend(title.position = "top")
    ) +
    theme(legend.position = "bottom")
)

# Use ggplot2::ggsave, not cowplot::ggsave
ggplot2::ggsave(glue("{OUT_STUB}_LEGEND.png"), cowplot::ggdraw(leg), width = 6, height = 1.6, dpi = 600)
ggplot2::ggsave(glue("{OUT_STUB}_LEGEND.svg"), cowplot::ggdraw(leg), width = 6, height = 1.6)
