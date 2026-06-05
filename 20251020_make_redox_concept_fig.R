# =========================
# Conceptual redox model — journal-quality multipanel
# =========================
# Packages
library(tidyverse)
suppressPackageStartupMessages({
  library(ggalluvial)   # install.packages("ggalluvial")
  library(patchwork)    # install.packages("patchwork")
  library(ggtext)       # install.packages("ggtext")
  # library(igraph); library(ggraph)  # uncomment if you want Panel C network
})

# ---------- Read & checks ----------
tbl <- read_csv("redox_concept_table.csv", show_col_types = FALSE)

required <- c("element","pH_band","ligand_context","soil_matrix",
              "oxic_state","reducing_state",
              "dominant_phase_oxic","dominant_phase_reducing","key_process","notes")
stopifnot(all(required %in% names(tbl)))

# Orderings (edit to taste)
element_order <- c("Fe","Mn","As","Sb","Cr","Tc","Pu","U","S","Se","V")
ligand_order  <- c("none","organic-rich","sulfidic")
tbl <- tbl %>%
  mutate(element = factor(element, levels = element_order),
         ligand_context = factor(ligand_context, levels = ligand_order))

# A colorblind-safe palette by element (Okabe–Ito-ish)
pal <- c(
  Fe="#56B4E9", Mn="#E69F00", As="#009E73", Sb="#CC79A7", Cr="#0072B2",
  Tc="#D55E00", Pu="#F0E442", U="#999999", S="#0099CC", Se="#33CC33", V="#AA4499"
)

# ---------- Panel A: Small-multiples alluvial by pH × ligand ----------
alluvial_df <- tbl %>%
  transmute(element, pH_band, ligand_context,
            left  = paste0("Oxic: ", oxic_state),
            right = paste0("Reducing: ", reducing_state),
            y = 1)

lab_lig <- c("none"="Ligands: none", "organic-rich"="Ligands: organic-rich", "sulfidic"="Ligands: S²⁻ present")

p_alluvial <- ggplot(alluvial_df,
                     aes(axis1 = left, axis2 = right, y = y)) +
  geom_alluvium(aes(fill = element), width = 1/12, alpha = 0.85, color = NA) +
  geom_stratum(width = 1/12, fill = "grey97", color = "grey60") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.9, lineheight = 0.98) +
  scale_fill_manual(values = pal, drop = FALSE) +
  scale_x_discrete(limits = c("Oxic","Reducing"), expand = c(.08,.08)) +
  facet_grid(ligand_context ~ pH_band, labeller = labeller(ligand_context = lab_lig)) +
  labs(title = "Conceptual redox transitions in soils (Oxic \u2192 Reducing)",
       subtitle = "Faceted by pH band and ligand context; flows colored by element",
       x = NULL, y = NULL, fill = "Element") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10)
  )

# ---------- Panel B: Tile matrix by soil matrix ----------
states <- tbl %>%
  pivot_longer(c(dominant_phase_oxic, dominant_phase_reducing),
               names_to = "condition", values_to = "phase") %>%
  mutate(condition = if_else(condition == "dominant_phase_oxic","Oxic","Reducing"),
         condition = factor(condition, levels = c("Reducing","Oxic")))

p_tiles <- ggplot(states,
                  aes(x = element, y = interaction(soil_matrix, condition, sep=" \u2022 "), label = phase)) +
  geom_tile(aes(fill = condition), width = 0.95, height = 0.9, color = "white") +
  geom_text(size = 3, lineheight = 0.98) +
  scale_fill_manual(values = c("Oxic" = "#dceef7", "Reducing" = "#fde2dd")) +
  labs(title = "Dominant phases by soil matrix and condition",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 1),
    plot.title = element_text(face = "bold", size = 12)
  )

# ---------- OPTIONAL Panel C: Sorbent & ligand coupling network ----------
# edges <- tribble(
#   ~from,            ~to,              ~type,
#   "Fe(III) oxides", "As(V)",          "sorption",
#   "Fe(III) oxides", "Se(IV)",         "sorption",
#   "Fe(III) oxides", "V(V)",           "sorption",
#   "Mn(IV) oxides",  "As(III)\u2192As(V)","abiotic oxidation",
#   "S\u00B2\u207B (sulfide)",  "Fe(II)\u2192FeS",   "precipitation",
#   "S\u00B2\u207B (sulfide)",  "U(VI)\u2192U(IV)",  "reduction",
#   "Organic ligands","Fe(III) oxides","ligand dissolution",
#   "Organic ligands","Pu(V/VI)",      "complexation"
# )
# g <- igraph::graph_from_data_frame(edges, directed = TRUE)
# p_net <- ggraph::ggraph(g, layout = "fr") +
#   ggraph::geom_edge_link(aes(linetype = type),
#                          arrow = arrow(length = unit(3,"mm")),
#                          edge_width = 0.5) +
#   ggraph::geom_node_point(size = 4) +
#   ggraph::geom_node_text(aes(label = name), vjust = 1.6, size = 3.3) +
#   theme_void() +
#   labs(title = "Couplings: Fe/Mn oxides, sulfide, and organic ligands") +
#   theme(plot.title = element_text(face = "bold", size = 12))

# ---------- Assemble ----------
# If Panel C enabled: final_fig <- p_alluvial / (p_tiles | p_net) + plot_layout(heights = c(3,2))
final_fig <- p_alluvial / p_tiles + plot_layout(heights = c(3,2))

# ---------- Export (journal quality) ----------
ggsave("conceptual_redox_model.svg", final_fig, width = 10.5, height = 9.0)
ggsave("conceptual_redox_model.png", final_fig, width = 10.5, height = 9.0, dpi = 600)
