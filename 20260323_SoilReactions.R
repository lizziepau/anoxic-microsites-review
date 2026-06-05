setwd("/Users/epaulus/Library/Mobile Documents/com~apple~CloudDocs/Manuscripts/2026_Paulus/LOAMS Review/LOAMS_Concept Model/Working Reaction Network")
getwd()

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(patchwork)
})


# ---------------------------------------------------------
# 1. Load Data
# ---------------------------------------------------------
df <- read_csv("RedoxTable.csv") %>%
  select(-starts_with("Unnamed")) %>%
  mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
  filter(!if_all(everything(), is.na))

# ---------------------------------------------------------
# 2. Normalize environments
# ---------------------------------------------------------
df <- df %>%
  mutate(
    env_react_raw = tolower(reactant_environment),
    env_prod_raw  = tolower(product_environment),
    
    env_react = if_else(env_react_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    env_prod  = if_else(env_prod_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    
    env_react = factor(env_react, levels = c("Oxic soil","Anoxic microsite")),
    env_prod  = factor(env_prod,  levels = c("Oxic soil","Anoxic microsite"))
  )

# ---------------------------------------------------------
# 3. Mediator classification
# ---------------------------------------------------------
sort(unique(tolower(df$mediator)))

df <- df %>%
  mutate(
    med_clean = tolower(str_squish(mediator)),
    
    mediator_mode = case_when(
      # biotic first
      str_detect(med_clean, "microbe|bacteria|srb|om|organic") ~ "biotic",
      
      # mineral-mediated second
      str_detect(med_clean, "mineral|oxide|mn|fe|fes|pyrite|sulfide|carbonate") ~ "mineral-mediated",
      
      # explicitly abiotic
      str_detect(med_clean, "oxygen|abiotic") ~ "abiotic",
      
      # fallback for everything else
      TRUE ~ "abiotic"
    ),
    
    mediator_mode = factor(
      mediator_mode,
      levels = c("abiotic", "biotic", "mineral-mediated")
    )
  )

linetype_palette <- c(
  abiotic = "solid",
  biotic = "dotdash",
  `mineral-mediated` = "dotted"
)

# ---------------------------------------------------------
# 4. Mobility effect
# ---------------------------------------------------------
df <- df %>%
  mutate(
    mobility_effect = tolower(mobility_effect),
    mobility_effect = if_else(
      mobility_effect %in% c("increase","decrease"),
      mobility_effect, NA_character_
    )
  ) %>%
  filter(!is.na(mobility_effect))

mobility_palette <- c(
  increase = "slateblue3",
  decrease = "darkcyan"
)

# ---------------------------------------------------------
# 5. Redox state fill
# ---------------------------------------------------------
state_levels <- c("reduced","moderately reduced","neutral",
                  "moderately oxidized","oxidized")

state_palette <- c(
  "reduced"             = "#87CEFF",
  "moderately reduced"  = "slategray1",
  "neutral"             = "#FAFAFA",
  "moderately oxidized" = "#FFC1C1",
  "oxidized"            = "#FA8072"
)

df <- df %>%
  mutate(
    reactant_state = factor(tolower(reactant_state), levels = state_levels),
    product_state  = factor(tolower(product_state),  levels = state_levels),
    label_react    = redox_reactant,
    label_prod     = redox_product
  )

# ---------------------------------------------------------
# 6. Layout
# ---------------------------------------------------------
x_oxic   <- 1.0
x_anoxic <- 2.0

df <- df %>%
  group_by(element) %>%
  mutate(
    y = seq(from = n(), to = 1, length.out = n()),
    
    x_react = if_else(env_react == "Oxic soil", x_oxic, x_anoxic),
    x_prod  = if_else(env_prod  == "Oxic soil", x_oxic, x_anoxic),
    
    # shorten arrows so arrowheads do not touch labels
    x_react_adj = x_react + if_else(x_react < x_prod,  0.25, -0.25),
    x_prod_adj  = x_prod  + if_else(x_react < x_prod, -0.25,  0.25)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 7. Background shading (no fill scale conflicts)
# ---------------------------------------------------------
bg_oxic  <- "#f6dc63"
bg_anox  <- "#698B69"
# ---------------------------
# GRADIENT BACKGROUND PATCH
# ---------------------------

# Function: create left→right gradient colors
#make_gradient <- function(col_left, col_right, n = 300) {
  
 # left  <- col2rgb(col_left)
#  right <- col2rgb(col_right)
  
#  grad <- sapply(1:3, function(i) seq(left[i], right[i], length.out = n))
  
#  rgb(t(grad), maxColorValue = 255)
#}

# Build gradient across 0.5 → 2.5 (oxic → anoxic)
#gradient_cols <- make_gradient("#f6e8c3", "#c7e9f1", n = 300)
#gradient_mat  <- matrix(gradient_cols, nrow = 1)

# Layer you can add into ggplot()
#gradient_background <- annotation_raster(
#  gradient_mat,
#  xmin = 0.5, xmax = 2.5,   # full width of your facet space
#  ymin = -Inf, ymax = Inf
#)
# ---------------------------------------------------------
# 8. Final Plot
# ---------------------------------------------------------
p <- ggplot(df) +
 # gradient_background +       
  # Backgrounds drawn inside each facet
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           fill = bg_oxic, alpha = 0.30) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
           fill = bg_anox, alpha = 0.30) +
  
  geom_vline(xintercept = 1.5, color="grey30", linewidth=0.5) +
  
  # Reaction line
  geom_segment(
    aes(
      x = x_react_adj, xend = x_prod_adj,
      y = y, yend = y,
      color = mobility_effect,
      linetype = mediator_mode
    ),
    linewidth = 0.9,
    lineend = "round"
  ) +
  
  # Arrowhead (always solid)
  geom_segment(
    aes(
      x = x_prod_adj,
      xend = x_prod_adj + if_else(x_prod > x_react, 0.07, -0.07),
      y = y, yend = y,
      color = mobility_effect
    ),
    arrow = arrow(type = "closed", length = unit(4, "mm")),
    linewidth = 0.9
  ) +
  
  # Reactant labels (centered inside environment block)
  geom_label(
    aes(x = x_react,
        y = y,
        label = label_react,
        fill = reactant_state),
    size = 3.6
  ) +
  
  # Product labels (centered inside environment block)
  geom_label(
    aes(x = x_prod,
        y = y,
        label = label_prod,
        fill = product_state),
    size = 3.6
  ) +
  
  facet_wrap(~ element, ncol = 3, scales = "free_y") +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Oxic soil","Anoxic microsite"),
    limits = c(0.5, 2.5)
  ) +
  
  scale_color_manual(values = mobility_palette, name = "Mobility Effect") +
  scale_linetype_manual(values = linetype_palette, name = "Mediator") +
 # scale_fill_manual(values = state_palette, name = "Redox State") +
  scale_fill_manual(
    name = "Redox state",
    values = state_palette,
    guide = guide_legend(override.aes = list(label = ""))
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  
  labs(
    title = "Redox transformations across the oxic–anoxic interface",
    subtitle = "Reactant and product positions determined from reactant_environment and product_environment"
  ) +
  
  coord_cartesian(clip = "off")

p

#####################################################################################################################
#####################################################################################################################

# --- Extract legend into its own panel ---
library(cowplot)

legend_only <- cowplot::get_legend(
  p +
    guides(fill = guide_legend(override.aes = list(label = ""))) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
)


# --- Main figure without legend ---
p_main <- p + theme(legend.position = "none")

# --- Save main figure ---
ggsave(
  "redox_main.png",
  p_main,
  width = 10, height = 14, dpi = 300
)

# --- Save legend as a standalone panel ---
legend_panel <- cowplot::ggdraw(legend_only)

ggsave(
  "redox_legend.png",
  legend_panel,
  width = 4, height = 4, dpi = 300
)










#########################################################################################################################################################
######### Adding mobility-mediator labels above arrows ##################################################################################################
#########################################################################################################################################################
setwd("/Users/epaulus/Library/Mobile Documents/com~apple~CloudDocs/Manuscripts/2026_Paulus/LOAMS Review/LOAMS_Concept Model/Working Reaction Network")
getwd()

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(patchwork)
})


# ---------------------------------------------------------
# 1. Load Data
# ---------------------------------------------------------
df <- read_csv("RedoxTable.csv") %>%
  select(-starts_with("Unnamed")) %>%
  mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
  filter(!if_all(everything(), is.na))

# ---------------------------------------------------------
# 2. Normalize environments
# ---------------------------------------------------------
df <- df %>%
  mutate(
    env_react_raw = tolower(reactant_environment),
    env_prod_raw  = tolower(product_environment),
    
    env_react = if_else(env_react_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    env_prod  = if_else(env_prod_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    
    env_react = factor(env_react, levels = c("Oxic soil","Anoxic microsite")),
    env_prod  = factor(env_prod,  levels = c("Oxic soil","Anoxic microsite"))
  )

# ---------------------------------------------------------
# 2B. Element order and grouping
# ---------------------------------------------------------
group1 <- c("Fe", "Mn", "As", "Sb")
group2 <- c("U", "Tc", "Pu", "Cr", "Se", "V")
group3 <- c("S")

element_order <- c(group1, group2, group3)

df <- df %>%
  mutate(
    element = factor(element, levels = element_order)
  )

# ---------------------------------------------------------
# 3. Mediator classification
# ---------------------------------------------------------
sort(unique(tolower(df$mediator)))

df <- df %>%
  mutate(
    med_clean = tolower(str_squish(mediator)),
    
    mediator_mode = case_when(
      # biotic first
      str_detect(med_clean, "microbe|bacteria|srb|om|organic") ~ "biotic",
      
      # mineral-mediated second
      str_detect(med_clean, "mineral|oxide|mn|fe|fes|pyrite|sulfide|carbonate") ~ "mineral-mediated",
      
      # explicitly abiotic
      str_detect(med_clean, "oxygen|abiotic") ~ "abiotic",
      
      # fallback for everything else
      TRUE ~ "abiotic"
    ),
    
    mediator_mode = factor(
      mediator_mode,
      levels = c("abiotic", "biotic", "mineral-mediated")
    )
  )

linetype_palette <- c(
  abiotic = "solid",
  biotic = "dotdash",
  `mineral-mediated` = "dotted"
)

# ---------------------------------------------------------
# 4. Mobility effect
# ---------------------------------------------------------
df <- df %>%
  mutate(
    mobility_effect = tolower(mobility_effect),
    mobility_effect = if_else(
      mobility_effect %in% c("increase","decrease"),
      mobility_effect, NA_character_
    )
  ) %>%
  filter(!is.na(mobility_effect))

mobility_palette <- c(
  increase = "slateblue3",
  decrease = "darkcyan"
)

# ---------------------------------------------------------
# 5. Redox state fill
# ---------------------------------------------------------
state_levels <- c("reduced","moderately reduced","neutral",
                  "moderately oxidized","oxidized")

state_palette <- c(
  "reduced"             = "#87CEFF",
  "moderately reduced"  = "slategray1",
  "neutral"             = "#FAFAFA",
  "moderately oxidized" = "#FFC1C1",
  "oxidized"            = "#FA8072"
)

df <- df %>%
  mutate(
    reactant_state = factor(tolower(reactant_state), levels = state_levels),
    product_state  = factor(tolower(product_state),  levels = state_levels),
    label_react    = redox_reactant,
    label_prod     = redox_product
  )

# ---------------------------------------------------------
# 6. Layout
# ---------------------------------------------------------
x_oxic   <- 1.0
x_anoxic <- 2.0

df <- df %>%
  group_by(element) %>%
  mutate(
    y = seq(from = n(), to = 1, length.out = n()),
    
    x_react = if_else(env_react == "Oxic soil", x_oxic, x_anoxic),
    x_prod  = if_else(env_prod  == "Oxic soil", x_oxic, x_anoxic),
    
    # shorten arrows so arrowheads do not touch labels
    x_react_adj = x_react + if_else(x_react < x_prod,  0.25, -0.25),
    x_prod_adj  = x_prod  + if_else(x_react < x_prod, -0.25,  0.25)
  ) %>%
  ungroup()

df <- df %>%
  group_by(element) %>%
  mutate(
    y = seq(from = n(), to = 1, length.out = n()),
    
    x_react = if_else(env_react == "Oxic soil", x_oxic, x_anoxic),
    x_prod  = if_else(env_prod  == "Oxic soil", x_oxic, x_anoxic),
    
    # shorten arrows so arrowheads do not touch labels
    x_react_adj = x_react + if_else(x_react < x_prod,  0.25, -0.25),
    x_prod_adj  = x_prod  + if_else(x_react < x_prod, -0.25,  0.25)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 6B. Add mediator label midpoints
# ---------------------------------------------------------
df <- df %>%
  mutate(
    x_mid = (x_react_adj + x_prod_adj) / 2,
    y_mid = y + 0.25,                 # vertical offset above arrows
    mediator_label = mediator
  )

names(df)



# ---------------------------------------------------------
# 7. Background shading (no fill scale conflicts)
# ---------------------------------------------------------
bg_oxic  <- "#f6dc63"
bg_anox  <- "#698B69"
# ---------------------------
# GRADIENT BACKGROUND PATCH
# ---------------------------

# Function: create left→right gradient colors
#make_gradient <- function(col_left, col_right, n = 300) {

# left  <- col2rgb(col_left)
#  right <- col2rgb(col_right)

#  grad <- sapply(1:3, function(i) seq(left[i], right[i], length.out = n))

#  rgb(t(grad), maxColorValue = 255)
#}

# Build gradient across 0.5 → 2.5 (oxic → anoxic)
#gradient_cols <- make_gradient("#f6e8c3", "#c7e9f1", n = 300)
#gradient_mat  <- matrix(gradient_cols, nrow = 1)

# Layer you can add into ggplot()
#gradient_background <- annotation_raster(
#  gradient_mat,
#  xmin = 0.5, xmax = 2.5,   # full width of your facet space
#  ymin = -Inf, ymax = Inf
#)
# ---------------------------------------------------------
# 8. Final Plot
# ---------------------------------------------------------
#p <- ggplot(df) +
  
 # annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
#           fill = bg_oxic, alpha = 0.30) +
 # annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
  #         fill = bg_anox, alpha = 0.30) +
  
#  geom_vline(xintercept = 1.5, color="grey30", linewidth=0.5) +
  
  # Reaction arrow
 # geom_segment(
  #  aes(x = x_react_adj, xend = x_prod_adj,
     #   y = y,           yend = y,
    #    color = mobility_effect,
   #     linetype = mediator_mode),
  #  linewidth = 0.9
 # ) +
  
  # Arrowhead
#  geom_segment(
#    aes(
#      x = x_prod_adj,
#      xend = x_prod_adj + if_else(x_prod > x_react, 0.07, -0.07),
#      y = y, yend = y,
#      color = mobility_effect
#    ),
  #  arrow = arrow(type = "closed", length = unit(4, "mm")),
 #   linewidth = 0.9
#  ) +
  
  # --- NEW: Mediator label ---
#  geom_text(
 #   aes(
  #    x = x_mid,
   #   y = y_mid,
    #  label = mediator_label,
     # color = mobility_effect   # or "black"
#    ),
 #   size = 3.3,
   # fontface = "italic",
  #  vjust = 0
#  ) +
  
  # Reactant label
#  geom_label(
#    aes(x = x_react,
#        y = y,
#        label = label_react,
#        fill = reactant_state),
#    size = 3.6
#  ) +
  
  # Product label
 # geom_label(
 #   aes(x = x_prod,
 #       y = y,
 #       label = label_prod,
#        fill = product_state),
#    size = 3.6
#  ) +
  
#  facet_wrap(~ element, ncol = 3, scales = "free_y") +
  
#  scale_x_continuous(
#    breaks = c(1, 2),
#    labels = c("Oxic soil","Anoxic microsite"),
#    limits = c(0.5, 2.5)
#  ) +
  
 # scale_color_manual(values = mobility_palette, name = "Mobility Effect") +
#  scale_linetype_manual(values = linetype_palette, name = "Mediator") +
  # scale_fill_manual(values = state_palette, name = "Redox State") +
#  scale_fill_manual(
  #  name = "Redox state",
 #   values = state_palette,
#    guide = guide_legend(override.aes = list(label = ""))
#  ) +
  
#  theme_minimal(base_size = 13) +
#  theme(
   # panel.grid = element_blank(),
  #  axis.title = element_blank(),
 #   axis.text.y = element_blank(),
#    axis.ticks = element_blank(),
   # axis.text.x = element_text(size = 11, face = "bold"),
  #  strip.text = element_text(size = 14, face = "bold"),
 #   legend.position = "bottom"
#  ) +
  
#  labs(
#    title = "Redox transformations across the oxic–anoxic interface",
#    subtitle = "Reactant and product positions determined from reactant_environment and product_environment"
 # ) +
  
 # coord_cartesian(clip = "off")

#p

# ---------------------------------------------------------
# 8. Final Plot
# ---------------------------------------------------------

base_plot <- ggplot(df) +
  
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
           fill = bg_oxic, alpha = 0.30) +
  annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
           fill = bg_anox, alpha = 0.30) +
  
  geom_vline(xintercept = 1.5, color = "grey30", linewidth = 0.5) +
  
  # Reaction arrow
  geom_segment(
    aes(x = x_react_adj, xend = x_prod_adj,
        y = y,           yend = y,
        color = mobility_effect,
        linetype = mediator_mode),
    linewidth = 0.9
  ) +
  
  # Arrowhead
  geom_segment(
    aes(
      x = x_prod_adj,
      xend = x_prod_adj + if_else(x_prod > x_react, 0.07, -0.07),
      y = y, yend = y,
      color = mobility_effect
    ),
    arrow = arrow(type = "closed", length = unit(4, "mm")),
    linewidth = 0.9
  ) +
  
  # Mediator label
  geom_text(
    aes(
      x = x_mid,
      y = y_mid,
      label = mediator_label,
      color = mobility_effect
    ),
    size = 3.3,
    vjust = 0
  ) +
  
  # Reactant label
  geom_label(
    aes(x = x_react,
        y = y,
        label = label_react,
        fill = reactant_state),
    size = 3.6
  ) +
  
  # Product label
  geom_label(
    aes(x = x_prod,
        y = y,
        label = label_prod,
        fill = product_state),
    size = 3.6
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Oxic soil", "Anoxic microsite"),
    limits = c(0.5, 2.5)
  ) +
  
  scale_color_manual(values = mobility_palette, name = "Mobility Effect") +
  scale_linetype_manual(values = linetype_palette, name = "Mediator") +
  scale_fill_manual(
    name = "Redox state",
    values = state_palette,
    guide = guide_legend(override.aes = list(label = ""))
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  
  labs(
    title = "Redox transformations across the oxic–anoxic interface",
    subtitle = "Reactant and product positions determined from reactant_environment and product_environment"
  ) +
  
  coord_cartesian(clip = "off")

# --- build grouped rows ---
p_row1 <- base_plot +
  facet_wrap(~ element, ncol = 4, scales = "free_y") +
  dplyr::filter(df, element %in% group1)

p_row2 <- base_plot +
  facet_wrap(~ element, ncol = 6, scales = "free_y") +
  dplyr::filter(df, element %in% group2)

p_row3 <- base_plot +
  facet_wrap(~ element, ncol = 1, scales = "free_y") +
  dplyr::filter(df, element %in% group3)

# Combine rows
p <- p_row1 / p_row2 / p_row3 +
  plot_layout(heights = c(1, 1, 0.6), guides = "collect") &
  theme(legend.position = "bottom")

p

#####################################################################################################################
#####################################################################################################################

# --- Extract legend into its own panel ---
#library(cowplot)

#legend_only <- cowplot::get_legend(
 # p +
  #  guides(fill = guide_legend(override.aes = list(label = ""))) +
   # theme(
    #  legend.position = "right",
     # legend.title = element_text(size = 14),
      #legend.text = element_text(size = 12)
#    )
#)


# --- Main figure without legend ---
#p_main <- p + theme(legend.position = "none")

# --- Save main figure ---
#ggsave(
 # "redox_main2.png",
 # p_main,
#  width = 10, height = 14, dpi = 300
#)

# --- Save legend as a standalone panel ---
#legend_panel <- cowplot::ggdraw(legend_only)

#ggsave(
#  "redox_legend2.png",
#  legend_panel,
#  width = 4, height = 4, dpi = 300
#)

# --- Extract legend into its own panel ---
library(cowplot)

legend_only <- cowplot::get_legend(
  p +
    guides(fill = guide_legend(override.aes = list(label = ""))) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
)

# --- Main figure without legend ---
p_main <- p + theme(legend.position = "none")

# --- Save main figure ---
ggsave(
  "redox_main_grouped.png",
  p_main,
  width = 16, height = 14, dpi = 300
)

# --- Save legend as a standalone panel ---
legend_panel <- cowplot::ggdraw(legend_only)

ggsave(
  "redox_legend_grouped.png",
  legend_panel,
  width = 4, height = 4, dpi = 300
)













setwd("/Users/epaulus/Library/Mobile Documents/com~apple~CloudDocs/Manuscripts/2026_Paulus/LOAMS Review/LOAMS_Concept Model/Working Reaction Network")
getwd()

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(patchwork)
  library(cowplot)
  library(grid)
})

# ---------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------
df <- read_csv("RedoxTable.csv") %>%
  select(-starts_with("Unnamed")) %>%
  mutate(across(everything(), ~ str_squish(as.character(.)))) %>%
  filter(!if_all(everything(), is.na))

# ---------------------------------------------------------
# 2. Element order and grouping
# ---------------------------------------------------------
group1 <- c("Fe", "Mn", "As", "Sb")
group2 <- c("U", "Tc", "Pu", "Cr", "Se", "V")
group3 <- c("S")
element_order <- c(group1, group2, group3)

df <- df %>%
  mutate(
    element = factor(element, levels = element_order)
  )

# ---------------------------------------------------------
# 3. Normalize environments
# ---------------------------------------------------------
df <- df %>%
  mutate(
    env_react_raw = tolower(reactant_environment),
    env_prod_raw  = tolower(product_environment),
    
    env_react = if_else(env_react_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    env_prod  = if_else(env_prod_raw == "bulk oxic soil",
                        "Oxic soil", "Anoxic microsite"),
    
    env_react = factor(env_react, levels = c("Oxic soil", "Anoxic microsite")),
    env_prod  = factor(env_prod,  levels = c("Oxic soil", "Anoxic microsite"))
  )

# ---------------------------------------------------------
# 4. Mediator classification
# ---------------------------------------------------------
sort(unique(tolower(df$mediator)))

df <- df %>%
  mutate(
    med_clean = tolower(str_squish(mediator)),
    
    mediator_mode = case_when(
      str_detect(med_clean, "microbe|bacteria|srb|om|organic") ~ "biotic",
      str_detect(med_clean, "mineral|oxide|mn|fe|fes|pyrite|sulfide|carbonate") ~ "mineral-mediated",
      str_detect(med_clean, "oxygen|abiotic") ~ "abiotic",
      TRUE ~ "abiotic"
    ),
    
    mediator_mode = factor(
      mediator_mode,
      levels = c("abiotic", "biotic", "mineral-mediated")
    )
  )

linetype_palette <- c(
  abiotic = "solid",
  biotic = "dotdash",
  `mineral-mediated` = "dotted"
)

# ---------------------------------------------------------
# 5. Mobility effect
# ---------------------------------------------------------
df <- df %>%
  mutate(
    mobility_effect = tolower(mobility_effect),
    mobility_effect = if_else(
      mobility_effect %in% c("increase", "decrease"),
      mobility_effect, NA_character_
    )
  ) %>%
  filter(!is.na(mobility_effect))

mobility_palette <- c(
  increase = "slateblue3",
  decrease = "darkcyan"
)

# ---------------------------------------------------------
# 6. Redox state fill
# ---------------------------------------------------------
state_levels <- c(
  "reduced", "moderately reduced", "neutral",
  "moderately oxidized", "oxidized"
)

state_palette <- c(
  "reduced"             = "#87CEFF",
  "moderately reduced"  = "slategray1",
  "neutral"             = "#FAFAFA",
  "moderately oxidized" = "#FFC1C1",
  "oxidized"            = "#FA8072"
)

df <- df %>%
  mutate(
    reactant_state = factor(tolower(reactant_state), levels = state_levels),
    product_state  = factor(tolower(product_state),  levels = state_levels),
    label_react    = redox_reactant,
    label_prod     = redox_product
  )

# ---------------------------------------------------------
# 7. Layout coordinates
# ---------------------------------------------------------
x_oxic   <- 1.0
x_anoxic <- 2.0

df <- df %>%
  group_by(element) %>%
  mutate(
    y = seq(from = n(), to = 1, length.out = n()),
    
    x_react = if_else(env_react == "Oxic soil", x_oxic, x_anoxic),
    x_prod  = if_else(env_prod  == "Oxic soil", x_oxic, x_anoxic),
    
    # shorten arrows so arrowheads do not touch labels
    x_react_adj = x_react + if_else(x_react < x_prod,  0.25, -0.25),
    x_prod_adj  = x_prod  + if_else(x_react < x_prod, -0.25,  0.25),
    
    # mediator label midpoint
    x_mid = (x_react_adj + x_prod_adj) / 2,
    y_mid = y + 0.25,
    mediator_label = mediator
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 8. Background colors
# ---------------------------------------------------------
bg_oxic <- "#f6dc63"
bg_anox <- "#698B69"

# ---------------------------------------------------------
# 9. Panel plotting function
# ---------------------------------------------------------
make_panel <- function(data, ncol) {
  ggplot(data) +
    
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf,
             fill = bg_oxic, alpha = 0.30) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf,
             fill = bg_anox, alpha = 0.30) +
    
    geom_vline(xintercept = 1.5, color = "grey30", linewidth = 0.5) +
    
    # Reaction arrow shaft
    geom_segment(
      aes(
        x = x_react_adj, xend = x_prod_adj,
        y = y, yend = y,
        color = mobility_effect,
        linetype = mediator_mode
      ),
      linewidth = 0.9
    ) +
    
    # Arrowhead
    geom_segment(
      aes(
        x = x_prod_adj,
        xend = x_prod_adj + if_else(x_prod > x_react, 0.07, -0.07),
        y = y, yend = y,
        color = mobility_effect
      ),
      arrow = arrow(type = "closed", length = unit(4, "mm")),
      linewidth = 0.9
    ) +
    
    # Mediator label
    geom_text(
      aes(
        x = x_mid,
        y = y_mid,
        label = mediator_label,
        color = mobility_effect
      ),
      size = 3.3,
      vjust = 0
    ) +
    
    # Reactant label
    geom_label(
      aes(
        x = x_react,
        y = y,
        label = label_react,
        fill = reactant_state
      ),
      size = 3.6
    ) +
    
    # Product label
    geom_label(
      aes(
        x = x_prod,
        y = y,
        label = label_prod,
        fill = product_state
      ),
      size = 3.6
    ) +
    
    facet_wrap(~ element, ncol = ncol, scales = "free_y") +
    
    scale_x_continuous(
      breaks = c(1, 2),
      labels = c("Oxic soil", "Anoxic microsite"),
      limits = c(0.5, 2.5)
    ) +
    
    scale_color_manual(values = mobility_palette, name = "Mobility Effect") +
    scale_linetype_manual(values = linetype_palette, name = "Mediator") +
    scale_fill_manual(
      name = "Redox state",
      values = state_palette,
      guide = guide_legend(override.aes = list(label = ""))
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 11, face = "bold"),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    ) +
    
    coord_cartesian(clip = "off")
}

# ---------------------------------------------------------
# 10. Build grouped figure
# ---------------------------------------------------------
p_row1 <- make_panel(filter(df, element %in% group1), ncol = 4)
p_row2 <- make_panel(filter(df, element %in% group2), ncol = 6)

# single centered S panel
p_s <- make_panel(filter(df, element %in% group3), ncol = 1) +
  theme(
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

p_row3 <- patchwork::plot_spacer() + p_s + patchwork::plot_spacer() +
  plot_layout(widths = c(1, 1.2, 1))

p <- p_row1 / p_row2 / p_row3 +
  plot_layout(heights = c(1, 1, 0.55), guides = "collect") &
  theme(legend.position = "bottom")

p <- p +
  plot_annotation(
    title = "Redox transformations across the oxic–anoxic interface",
    subtitle = "Reactant and product positions determined from reactant_environment and product_environment"
  )

p

# ---------------------------------------------------------
# 11. Custom legend panel - horizontal 3-column layout
# ---------------------------------------------------------
library(ggplot2)
library(grid)
library(tibble)

# Column 1: Mobility effect
legend_df_arrows <- tibble(
  x = 0.2,
  xend = 1.4,
  y = c(2.2, 1.4),
  label = c("Decrease mobility", "Increase mobility"),
  col = c("decrease", "increase")
)

# Column 2: Mediator
legend_df_mediator <- tibble(
  x = 0.2,
  xend = 1.4,
  y = c(2.2, 1.4, 0.6),
  label = c("Abiotic", "Biotic", "Mineral-mediated"),
  lty = c("abiotic", "biotic", "mineral-mediated")
)

# Column 3: Redox state
legend_df_boxes <- tibble(
  x = 0.35,
  y = c(2.6, 2.0, 1.4, 0.8, 0.2),
  label = c(
    "Reduced",
    "Moderately reduced",
    "Neutral",
    "Moderately oxidized",
    "Oxidized"
  ),
  fill = c(
    "reduced",
    "moderately reduced",
    "neutral",
    "moderately oxidized",
    "oxidized"
  )
)

# ---------------------------
# Mobility legend
# ---------------------------
legend_mobility <- ggplot() +
  annotate(
    "text", x = 0.2, y = 3.0,
    label = "Mobility effect",
    hjust = 0, size = 5, fontface = "bold"
  ) +
  geom_segment(
    data = legend_df_arrows,
    aes(x = x, xend = xend, y = y, yend = y, color = col),
    linewidth = 1.8,
    arrow = arrow(type = "closed", length = unit(5, "mm"))
  ) +
  geom_text(
    data = legend_df_arrows,
    aes(x = 1.75, y = y, label = label),
    hjust = 0, size = 4.4
  ) +
  scale_color_manual(
    values = c(
      decrease = unname(mobility_palette["decrease"]),
      increase = unname(mobility_palette["increase"])
    ),
    guide = "none"
  ) +
  coord_cartesian(xlim = c(0, 4.3), ylim = c(0, 3.2), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

# ---------------------------
# Mediator legend
# ---------------------------
legend_mediator <- ggplot() +
  annotate(
    "text", x = 0.2, y = 3.0,
    label = "Mediator",
    hjust = 0, size = 5, fontface = "bold"
  ) +
  geom_segment(
    data = legend_df_mediator,
    aes(x = x, xend = xend, y = y, yend = y, linetype = lty),
    linewidth = 1.4,
    color = "black"
  ) +
  geom_text(
    data = legend_df_mediator,
    aes(x = 1.75, y = y, label = label),
    hjust = 0, size = 4.4
  ) +
  scale_linetype_manual(
    values = c(
      abiotic = unname(linetype_palette["abiotic"]),
      biotic = unname(linetype_palette["biotic"]),
      `mineral-mediated` = unname(linetype_palette["mineral-mediated"])
    ),
    guide = "none"
  ) +
  coord_cartesian(xlim = c(0, 4.3), ylim = c(0, 3.2), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

# ---------------------------
# Redox legend
# ---------------------------
legend_redox <- ggplot() +
  annotate(
    "text", x = 0.2, y = 3.0,
    label = "Redox state",
    hjust = 0, size = 5, fontface = "bold"
  ) +
  geom_tile(
    data = legend_df_boxes,
    aes(x = x, y = y, fill = fill),
    width = 0.45,
    height = 0.45,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    data = legend_df_boxes,
    aes(x = 0.85, y = y, label = label),
    hjust = 0, size = 4.2
  ) +
  scale_fill_manual(
    values = c(
      reduced = unname(state_palette["reduced"]),
      `moderately reduced` = unname(state_palette["moderately reduced"]),
      neutral = unname(state_palette["neutral"]),
      `moderately oxidized` = unname(state_palette["moderately oxidized"]),
      oxidized = unname(state_palette["oxidized"])
    ),
    guide = "none"
  ) +
  coord_cartesian(xlim = c(0, 3.6), ylim = c(0, 3.2), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

# Combine into 3 horizontal columns
legend_plot <- legend_mobility + legend_mediator + legend_redox +
  plot_layout(widths = c(1.35, 1.2, 1.1))

legend_plot

# ---------------------------------------------------------
# 12. Save outputs
# ---------------------------------------------------------
p_main <- p & theme(legend.position = "none")

ggsave(
  "redox_main_grouped.png",
  p_main,
  width = 16, height = 14, dpi = 300, bg = "white"
)

ggsave(
  "redox_legend_custom.png",
  legend_plot,
  width = 6.5, height = 8.5, dpi = 300, bg = "white"
)

final_combined <- p_main | legend_plot +
  plot_layout(widths = c(4.2, 1.3))

ggsave(
  "redox_combined_with_custom_legend.png",
  final_combined,
  width = 18, height = 14, dpi = 300, bg = "white"
)

ggsave(
  "redox_legend_horizontal.png",
  legend_plot,
  width = 14,
  height = 3.8,
  dpi = 300,
  bg = "white"
)

final_combined <- p_main / legend_plot +
  plot_layout(heights = c(1, 0.22))

ggsave(
  "redox_combined_with_horizontal_legend.png",
  final_combined,
  width = 16,
  height = 15,
  dpi = 300,
  bg = "white"
)

