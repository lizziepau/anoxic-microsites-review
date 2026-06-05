setwd("~/Documents/LOAMS_Review/ReactionNetwork")
getwd()
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

# ------------------------------------------------------------
# 1. Load and clean data
# ------------------------------------------------------------
redox_df <- read_csv("RedoxTable.csv") %>%
  filter(!if_all(everything(), ~ is.na(.))) %>%
  mutate(across(everything(), ~ str_squish(as.character(.))))

# Ensure element column exists
stopifnot("element" %in% names(redox_df))

element_levels <- c("As","Cr","Fe","Mn","Pu","Sb","Se","Tc","U","V","S")
redox_df$element <- factor(redox_df$element, levels = element_levels)

# ------------------------------------------------------------
# 2. Normalize environment labels
# ------------------------------------------------------------
redox_df <- redox_df %>%
  mutate(
    env_lower = tolower(environment),
    environment_clean = case_when(
      str_detect(env_lower, "bulk") & str_detect(env_lower, "oxic")
      ~ "bulk oxic soil",
      str_detect(env_lower, "anoxic")
      ~ "anoxic microsite",
      TRUE ~ environment
    ),
    environment_clean = factor(
      environment_clean,
      levels = c("bulk oxic soil", "anoxic microsite")
    )
  )

# ------------------------------------------------------------
# 3. Classify mediator_mode
# ------------------------------------------------------------
redox_df <- redox_df %>%
  mutate(
    mediator_lower = tolower(mediator),
    mediator_mode = case_when(
      str_detect(mediator_lower, "microbe|bacteria|srb") ~ "biotic",
      str_detect(mediator_lower,
                 "oxide|mineral|sulfide|fes|fes2|pyrite|goethite|ferrihydrite|mackinawite|mn")
      ~ "mineral-mediated",
      TRUE ~ "abiotic"
    ),
    mediator_mode = factor(mediator_mode,
                           levels = c("abiotic", "biotic", "mineral-mediated"))
  )

linetype_palette <- c(
  abiotic = "solid",
  biotic  = "longdash",
  `mineral-mediated` = "dotted"
)

# ------------------------------------------------------------
# 4. Classify mobility effect
# ------------------------------------------------------------
redox_df <- redox_df %>%
  mutate(
    mobility_effect = tolower(str_squish(mobility_effect)),
    mobility_effect = case_when(
      str_detect(mobility_effect, "decrease") ~ "decrease",
      str_detect(mobility_effect, "increase") ~ "increase",
      TRUE ~ "neutral"
    ),
    mobility_effect = factor(
      mobility_effect,
      levels = c("increase", "decrease", "neutral")
    )
  )

mobility_palette <- c(
  increase = "lightsteelblue1",
  decrease = "lightsalmon",
  neutral  = "grey80"
)

# ------------------------------------------------------------
# 5. Create deterministic vertical spacing WITHIN each element
# ------------------------------------------------------------
plot_df <- redox_df %>%
  arrange(element, environment_clean) %>%
  group_by(element, environment_clean) %>%
  mutate(reaction_index = row_number()) %>%
  ungroup()

# ------------------------------------------------------------
# 6. Function to generate a clean plot for 1 element
# ------------------------------------------------------------
plot_element <- function(el) {
  
  df_el <- plot_df %>% filter(element == el)
  
  p <- ggplot(df_el) +
    geom_segment(
      aes(
        x = 1.1, xend = 1.9,
        y = reaction_index, yend = reaction_index,
        linetype = mediator_mode
      ),
      linewidth = 0.7,
      color = "black",
      arrow = arrow(length = unit(3, "mm"), type = "closed")
    ) +
    geom_text(
      aes(x = 1, y = reaction_index, label = redox_reactant),
      hjust = 1,
      size = 5
    ) +
    geom_label(
      aes(
        x = 2.2,
        y = reaction_index,
        label = redox_product,
        fill = mobility_effect
      ),
      label.r = unit(0.25, "lines"),
      size = 5,
      linewidth = 0.3,
      color = "black"
    ) +
    facet_wrap(~ environment_clean, nrow = 1) +
    scale_fill_manual(values = mobility_palette) +
    scale_linetype_manual(values = linetype_palette) +
    xlim(0.5, 2.7) +
    scale_y_reverse() +
    theme_minimal(base_family = "Helvetica") +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
    ) +
    labs(
      title = paste("Redox reactions for", el)
    )
  
  return(p)
}

# ------------------------------------------------------------
# 7. Loop over elements and save a PNG for each
# ------------------------------------------------------------
for (el in element_levels) {
  df_el <- plot_df %>% filter(element == el)
  if (nrow(df_el) == 0) next
  
  p_el <- plot_element(el)
  
  ggsave(
    paste0("redox_network_", el, ".png"),
    p_el,
    width = 10,
    height = 5 + 0.5 * nrow(df_el),  # taller panels for more reactions
    dpi = 300
  )
}

