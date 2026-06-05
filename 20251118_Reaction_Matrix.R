#setwd("~/Documents/LOAMS_Review/ReactionNetwork")
getwd()

# ============================================================
# FULL FOUR-PANEL OXIC → ANOXIC REACTION DIAGRAM SCRIPT
# - Uses reaction_network_edges_EXPANDED.csv in WD
# - Rows = elements (Mn, Fe, S, As, Sb, Cr, Se, V, U, Pu, Tc)
# - Columns = A (element-only), B (+Fe), C (+Mn), D (+S)
# - Vertical grouping by species identity
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

# ------------------------------------------------------------
# 1. Load dataset safely
# ------------------------------------------------------------
edges_df <- read_delim(
  "reaction_network_edges_EXPANDED.csv",
  delim = ",",
  col_names = TRUE,
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  filter(!if_all(everything(), is.na)) %>%
  filter(environment %in% c("bulk_oxic", "anoxic_microsite")) %>%
  mutate(
    driver       = tolower(driver),
    process_type = tolower(process_type)
  )

# ------------------------------------------------------------
# 2. Species → element grouping
# ------------------------------------------------------------
nodes_df <- tibble(name = unique(c(edges_df$from, edges_df$to))) %>%
  mutate(
    element_group = case_when(
      str_detect(name, "^fe")                    ~ "Fe",
      str_detect(name, "^mn")                    ~ "Mn",
      str_detect(name, "(^s$|s0|so4|hs|s2-)")   ~ "S",
      str_detect(name, "^as")                    ~ "As",
      str_detect(name, "^sb")                    ~ "Sb",
      str_detect(name, "^cr")                    ~ "Cr",
      str_detect(name, "^se")                    ~ "Se",
      str_detect(name, "^v\\(")                  ~ "V",
      str_detect(name, "^u")                     ~ "U",
      str_detect(name, "^pu")                    ~ "Pu",
      str_detect(name, "^tc")                    ~ "Tc",
      TRUE                                      ~ "Other"
    )
  )

edges_ext <- edges_df %>%
  left_join(nodes_df, by = c("from" = "name")) %>%
  rename(from_group = element_group) %>%
  left_join(nodes_df, by = c("to" = "name")) %>%
  rename(to_group = element_group)

# ------------------------------------------------------------
# 3. Reaction mode: abiotic / biotic / mineral-mediated
# ------------------------------------------------------------
edges_ext <- edges_ext %>%
  mutate(
    mode = case_when(
      str_detect(driver, "bacteria|microbe|sulfate-reducing") ~ "biotic",
      str_detect(driver,
                 "mno2|oxide|fes|fes2|pyrite|mackinawite|mineral") ~ "mineral",
      TRUE ~ "abiotic"
    ),
    mode = factor(mode, levels = c("abiotic","biotic","mineral"))
  )

# ------------------------------------------------------------
# 4. Mobility classification
# ------------------------------------------------------------
edges_ext <- edges_ext %>%
  mutate(
    from_aq    = str_detect(from, "_aq|carbonate"),
    to_aq      = str_detect(to,   "_aq|carbonate"),
    from_solid = str_detect(from, "_s|sorbed|oxide|ox_s|fes|fes2"),
    to_solid   = str_detect(to,   "_s|sorbed|oxide|ox_s|fes|fes2")
  ) %>%
  mutate(
    mob_status = case_when(
      from_solid & to_aq ~ "mobilization",
      from_aq & to_solid ~ "immobilization",
      TRUE ~ "neutral"
    ),
    mob_status = factor(
      mob_status,
      levels = c("mobilization","immobilization","neutral")
    )
  )

mob_palette <- c(
  mobilization   = "#1b9e77",
  immobilization = "#8c510a",
  neutral        = "grey60"
)

mode_linetypes <- c(
  abiotic = "solid",
  biotic  = "longdash",
  mineral = "dotted"
)

# ------------------------------------------------------------
# 5. Fe / Mn / S mediation flags (SAFE VERSION)
# ------------------------------------------------------------
edges_ext <- edges_ext %>%
  mutate(
    # Fe mediation or involvement
    fe_in_edge = (
      from_group == "Fe" | to_group == "Fe" |
        str_detect(from, "fe") | str_detect(to, "fe") |
        str_detect(driver, "fe")
    ),
    
    # Mn mediation
    mn_in_edge = (
      from_group == "Mn" | to_group == "Mn" |
        str_detect(from, "mn") | str_detect(to, "mn") |
        str_detect(driver, "mn")
    ),
    
    # S mediation (free S + S minerals + microbial S)
    s_in_edge = (
      from_group == "S" | to_group == "S" |
        str_detect(from, "s0|so4|hs|s2-") |
        str_detect(to,   "s0|so4|hs|s2-") |
        str_detect(from, "fes|fes2|pyrite|mackinawite") |
        str_detect(to,   "fes|fes2|pyrite|mackinawite") |
        str_detect(driver, "sulfide|sulfate-reducing|s0|s2-")
    )
  )

# ------------------------------------------------------------
# 6. Build four panels per element
# ------------------------------------------------------------
row_elements <- c("Mn","Fe","S","As","Sb","Cr","Se","V","U","Pu","Tc")
panel_levels <- c("A_element_only","B_with_Fe","C_with_Mn","D_with_S")

build_for_element <- function(elem) {
  
  # broad detection of element involvement
  base_edges <- edges_ext %>%
    mutate(
      focal_in_fromto = (from_group == elem | to_group == elem),
      focal_in_driver = str_detect(driver, tolower(elem)),
      focal_in_species = focal_in_fromto | focal_in_driver
    ) %>%
    filter(focal_in_species)
  
  if (nrow(base_edges) == 0) return(tibble())
  
  A <- base_edges %>%
    filter(!(fe_in_edge | mn_in_edge | s_in_edge)) %>%
    mutate(panel_label = "A_element_only")
  
  B <- base_edges %>%
    filter(fe_in_edge) %>%
    mutate(panel_label = "B_with_Fe")
  
  C <- base_edges %>%
    filter(mn_in_edge) %>%
    mutate(panel_label = "C_with_Mn")
  
  D <- base_edges %>%
    filter(s_in_edge) %>%
    mutate(panel_label = "D_with_S")
  
  bind_rows(A,B,C,D) %>%
    mutate(row_element = elem)
}

panel_data <- map_dfr(row_elements, build_for_element)

if (nrow(panel_data) == 0) {
  stop("No reactions survived filtering. Check CSV format.")
}

panel_data <- panel_data %>%
  mutate(panel_label = factor(panel_label, levels = panel_levels))

# ------------------------------------------------------------
# 7. Vertical grouping by species identity (Option 2)
# ------------------------------------------------------------
species_group <- function(x) {
  case_when(
    str_detect(x, "^fe")                    ~ "Fe",
    str_detect(x, "^mn")                    ~ "Mn",
    str_detect(x, "(^s$|s0|so4|hs|s2-)")   ~ "S",
    str_detect(x, "^as")                    ~ "As",
    str_detect(x, "^sb")                    ~ "Sb",
    str_detect(x, "^cr")                    ~ "Cr",
    str_detect(x, "^se")                    ~ "Se",
    str_detect(x, "^v\\(")                  ~ "V",
    str_detect(x, "^u")                     ~ "U",
    str_detect(x, "^pu")                    ~ "Pu",
    str_detect(x, "^tc")                    ~ "Tc",
    TRUE                                    ~ "Other"
  )
}

panel_data <- panel_data %>%
  mutate(
    from_species_group = species_group(from),
    to_species_group   = species_group(to),
    vertical_block = case_when(
      from_species_group == row_element |
        to_species_group == row_element ~ "1_focal",
      from_species_group == "Fe" | to_species_group == "Fe" ~ "2_Fe",
      from_species_group == "Mn" | to_species_group == "Mn" ~ "3_Mn",
      from_species_group == "S"  | to_species_group == "S"  ~ "4_S",
      TRUE ~ "5_other"
    )
  ) %>%
  group_by(row_element, panel_label, vertical_block) %>%
  arrange(from, to, .by_group = TRUE) %>%
  mutate(local_order = row_number()) %>%
  ungroup() %>%
  arrange(row_element, panel_label, vertical_block, local_order) %>%
  group_by(row_element, panel_label) %>%
  mutate(y = -row_number()) %>%
  ungroup()

# ------------------------------------------------------------
# 8. X positions + interface shading
# ------------------------------------------------------------
panel_data <- panel_data %>%
  mutate(
    x_from        = 0.10,
    x_arrow_start = 0.30,
    x_arrow_end   = 0.70,
    x_to          = 0.90
  )

interface_band <- tibble(
  xmin = 0.45, xmax = 0.55,
  ymin = -Inf, ymax = Inf
)

# ------------------------------------------------------------
# 9. Final plot (facet grid)
# ------------------------------------------------------------
p <- ggplot(panel_data) +
  geom_rect(
    data = interface_band,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey92", color = NA, inherit.aes = FALSE
  ) +
  geom_segment(
    aes(
      x = x_arrow_start, xend = x_arrow_end,
      y = y, yend = y,
      color = mob_status,
      linetype = mode
    ),
    arrow = arrow(length = unit(2.5, "mm"), type = "closed"),
    size  = 0.6
  ) +
  geom_text(aes(x = x_from, y = y, label = from),   hjust = 0, size = 3) +
  geom_text(aes(x = x_to,   y = y, label = to),     hjust = 1, size = 3) +
  scale_color_manual(values = mob_palette) +
  scale_linetype_manual(values = mode_linetypes) +
  scale_x_continuous(
    limits = c(0,1),
    breaks = c(0.1, 0.5, 0.9),
    labels = c("Oxic region", "Interface", "Anoxic microsite")
  ) +
  facet_grid(
    rows = vars(row_element),
    cols = vars(panel_label),
    scales = "free_y",
    space  = "free_y"
  ) +
  theme_minimal(base_family="Helvetica") +
  theme(
    panel.grid       = element_blank(),
    strip.text       = element_text(size=10, face="bold"),
    strip.background = element_rect(fill="grey95", color=NA),
    axis.title       = element_blank(),
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    legend.position  = "bottom",
    plot.background  = element_rect(fill="white",color=NA)
  ) +
  labs(
    title = "Element-centered mobilization and immobilization across the oxic → anoxic microsite interface",
    subtitle = "Columns: A (element only), B (+Fe), C (+Mn), D (+S). Vertical grouping shows species identity blocks."
  )

print(p)

# Optional export:
ggsave(
  "element_four_panel_interface.png",
  p, width = 16, height = 27, units = "in", dpi = 350
)

