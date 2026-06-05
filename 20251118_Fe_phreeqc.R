############################################################
# Fe-only Pourbaix with kinetic overlay (PHREEQC + ggplot2)
# - Equilibrium: Fe2+, Fe3+, FeOH+, Fe(OH)2, Fe(OH)3(aq)
# - Minerals: Ferrihydrite, Goethite, Siderite
# - Oxic vs Anoxic grids (different pe, log fCO2)
# - Kinetic overlay: simple Eh_crit(pH) boundary
# - Fe–S minerals shown as *annotations* (not solved numerically)
############################################################

library(glue)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---------------------------------------------------------
# 0. PHREEQC paths (adjust for your system)
# ---------------------------------------------------------
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/Users/epaulus/Documents/LOAMS_Review/phreeqc/iphreeqc-3.8.6-17100/database/phreeqc.dat"


# ---------------------------------------------------------
# 1. Single-point Fe speciation with PHREEQC
# ---------------------------------------------------------
run_sim_fe <- function(pH_in,
                       pe_in,
                       Fe_tot   = 1e-6,
                       log_fCO2 = -2,
                       phreeqc_exe = PHREEQC_EXE,
                       db_path     = DB_PATH) {
  
  workdir <- tempdir()
  infile  <- file.path(workdir, paste0("fe_in_",  Sys.getpid(), ".pqi"))
  outfile <- file.path(workdir, paste0("fe_out_", Sys.getpid(), ".out"))
  selfile <- file.path(workdir, paste0("fe_sel_", Sys.getpid(), ".txt"))
  
  # PHREEQC input: Fe-only, with CO2(g) buffer; no sulfur here
  input_txt <- glue("
USER_PUNCH
    -headings pH_eq pe_eq Fe2 Fe3 FeOH FeOH2 FeOH3 SI_fh SI_go SI_sid
10  PUNCH -LA(\"H+\")
20  PUNCH -LA(\"e-\")
30  PUNCH MOL(\"Fe+2\")
40  PUNCH MOL(\"Fe+3\")
50  PUNCH MOL(\"FeOH+\")
60  PUNCH MOL(\"Fe(OH)2\")
70  PUNCH MOL(\"Fe(OH)3(aq)\")
80  PUNCH SI(\"Ferrihydrite\")
90  PUNCH SI(\"Goethite\")
100 PUNCH SI(\"Siderite\")
END

SELECTED_OUTPUT
    -file {selfile}
    -reset true
    -user_punch true
END

SOLUTION 1
    temp 25
    pH   {pH_in}
    pe   {pe_in}
    units mol/kgw
    Na   0.1
    Cl   0.1
    Fe   {Fe_tot}
END

EQUILIBRIUM_PHASES 1
    CO2(g) {log_fCO2}
")
  
  writeLines(input_txt, infile)
  
  # Silent PHREEQC call
  suppressWarnings(
    system2(phreeqc_exe, args = c(infile, outfile, db_path),
            stdout = FALSE, stderr = FALSE)
  )
  
  # If no selected output, return NA row
  if (!file.exists(selfile)) {
    return(tibble(
      pH_eq  = NA_real_,
      pe_eq  = NA_real_,
      Fe2    = NA_real_,
      Fe3    = NA_real_,
      FeOH   = NA_real_,
      FeOH2  = NA_real_,
      FeOH3  = NA_real_,
      SI_fh  = NA_real_,
      SI_go  = NA_real_,
      SI_sid = NA_real_
    ))
  }
  
  df <- suppressWarnings(
    readr::read_table(selfile, comment = "#", show_col_types = FALSE)
  )
  if (nrow(df) == 0) {
    return(tibble(
      pH_eq  = NA_real_,
      pe_eq  = NA_real_,
      Fe2    = NA_real_,
      Fe3    = NA_real_,
      FeOH   = NA_real_,
      FeOH2  = NA_real_,
      FeOH3  = NA_real_,
      SI_fh  = NA_real_,
      SI_go  = NA_real_,
      SI_sid = NA_real_
    ))
  }
  
  # Take last row (the equilibrated solution)
  last <- df %>%
    slice_tail(n = 1) %>%
    transmute(
      pH_eq  = as.numeric(pH_eq),
      pe_eq  = as.numeric(pe_eq),
      Fe2    = as.numeric(Fe2),
      Fe3    = as.numeric(Fe3),
      FeOH   = as.numeric(FeOH),
      FeOH2  = as.numeric(FeOH2),
      FeOH3  = as.numeric(FeOH3),
      SI_fh  = as.numeric(SI_fh),
      SI_go  = as.numeric(SI_go),
      SI_sid = as.numeric(SI_sid)
    )
  
  last
}


# ---------------------------------------------------------
# 2. Build a grid of (pH, pe) and classify Fe phases
# ---------------------------------------------------------
run_grid <- function(pH_vals,
                     pe_vals,
                     Fe_tot   = 1e-6,
                     log_fCO2 = -2) {
  
  base_grid <- tidyr::expand_grid(
    pH_in = pH_vals,
    pe_in = pe_vals
  )
  
  # Run PHREEQC for each grid point
  res_list <- purrr::map2(
    base_grid$pH_in,
    base_grid$pe_in,
    ~ run_sim_fe(.x, .y, Fe_tot = Fe_tot, log_fCO2 = log_fCO2)
  )
  
  res_df <- dplyr::bind_rows(res_list)
  
  out <- dplyr::bind_cols(base_grid, res_df) %>%
    # Derived quantities
    mutate(
      Eh_V   = 0.05916 * pe_eq,
      Fe_aq  = Fe2 + Fe3 + FeOH + FeOH2 + FeOH3,
      Fe2_frac = if_else(Fe_aq > 0, Fe2 / Fe_aq, NA_real_),
      Fe3_frac = if_else(Fe_aq > 0, Fe3 / Fe_aq, NA_real_),
      log_Fe2_Fe3 = log10(pmax(Fe2, 1e-30) / pmax(Fe3, 1e-30)),
      # Equilibrium phase classification
      phase_eq = case_when(
        !is.na(SI_sid) & SI_sid >= 0 ~ "Siderite",
        !is.na(SI_go)  & SI_go  >= 0 & SI_go >= SI_fh ~ "Goethite",
        !is.na(SI_fh)  & SI_fh  >= 0 ~ "Ferrihydrite",
        !is.na(Fe2_frac) & Fe2_frac >= 0.9 ~ "Fe2+ (aq)",
        !is.na(Fe3_frac) & Fe3_frac >= 0.9 ~ "Fe3+ (aq)",
        TRUE ~ "Mixed Fe"
      ),
      phase_eq = factor(
        phase_eq,
        levels = c("Fe2+ (aq)", "Mixed Fe", "Fe3+ (aq)",
                   "Ferrihydrite", "Goethite", "Siderite")
      ),
      # Simple kinetic critical Eh (tunable)
      Eh_crit = 0.2 + 0.03 * (pH_eq - 7),
      phase_kin = case_when(
        Eh_V < Eh_crit &
          phase_eq %in% c("Ferrihydrite", "Goethite", "Fe3+ (aq)") ~
          "Fe(II)-dominated (kinetic)",
        TRUE ~ as.character(phase_eq)
      ),
      phase_kin = factor(
        phase_kin,
        levels = c("Fe2+ (aq)", "Fe(II)-dominated (kinetic)",
                   "Mixed Fe", "Fe3+ (aq)",
                   "Ferrihydrite", "Goethite", "Siderite")
      )
    )
  
  out
}


# ---------------------------------------------------------
# 3. Helper: Fe palettes
# ---------------------------------------------------------
fe_palette_eq <- c(
  "Fe2+ (aq)"    = "#1b9e77",
  "Mixed Fe"     = "#7570b3",
  "Fe3+ (aq)"    = "#d95f02",
  "Ferrihydrite" = "#e6ab02",
  "Goethite"     = "#a6761d",
  "Siderite"     = "#66a61e"
)

fe_palette_kin <- c(
  "Fe2+ (aq)"                 = "#1b9e77",
  "Fe(II)-dominated (kinetic)"= "#66c2a5",
  "Mixed Fe"                  = "#7570b3",
  "Fe3+ (aq)"                 = "#d95f02",
  "Ferrihydrite"              = "#e6ab02",
  "Goethite"                  = "#a6761d",
  "Siderite"                  = "#66a61e"
)


# ---------------------------------------------------------
# 4. Plotting: equilibrium Pourbaix
# ---------------------------------------------------------
plot_pourbaix_eq <- function(grid_df,
                             title = "Fe Pourbaix – equilibrium",
                             add_sulfur_overlay = TRUE) {
  
  df <- grid_df %>%
    filter(is.finite(pH_eq), is.finite(Eh_V))
  
  p <- ggplot(df, aes(x = pH_eq, y = Eh_V)) +
    geom_tile(aes(fill = phase_eq)) +
    scale_fill_manual(
      values = fe_palette_eq,
      drop   = FALSE,
      name   = "Equilibrium Fe phase"
    ) +
    labs(
      title = title,
      x     = "pH (equilibrium)",
      y     = "Eh (V, 25 °C)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16)
    )
  
  # ---- Fe2+/Fe3+ = 1 boundary (if it exists) ----
  vals <- df$log_Fe2_Fe3[is.finite(df$log_Fe2_Fe3)]
  if (length(vals) > 0 && min(vals) < 0 && max(vals) > 0) {
    p <- p +
      stat_contour(
        aes(z = log_Fe2_Fe3),
        breaks    = 0,
        colour    = "black",
        linewidth = 0.5
      )
  }
  
  # ---- SI = 0 contours, only if they cross zero ----
  # Ferrihydrite
  vals_fh <- df$SI_fh[is.finite(df$SI_fh)]
  if (length(vals_fh) > 0 && min(vals_fh) <= 0 && max(vals_fh) >= 0) {
    p <- p + stat_contour(
      aes(z = SI_fh),
      breaks    = 0,
      colour    = "white",
      linetype  = "dashed",
      linewidth = 0.5
    )
  }
  # Goethite
  vals_go <- df$SI_go[is.finite(df$SI_go)]
  if (length(vals_go) > 0 && min(vals_go) <= 0 && max(vals_go) >= 0) {
    p <- p + stat_contour(
      aes(z = SI_go),
      breaks    = 0,
      colour    = "grey20",
      linetype  = "dotted",
      linewidth = 0.5
    )
  }
  # Siderite
  vals_sid <- df$SI_sid[is.finite(df$SI_sid)]
  if (length(vals_sid) > 0 && min(vals_sid) <= 0 && max(vals_sid) >= 0) {
    p <- p + stat_contour(
      aes(z = SI_sid),
      breaks    = 0,
      colour    = "goldenrod3",
      linetype  = "solid",
      linewidth = 0.6
    )
  }
  
  # ---- Qualitative Fe–S overlay (mackinawite, pyrite, etc.) ----
  if (add_sulfur_overlay) {
    # Mackinawite-ish field
    p <- p +
      annotate("rect",
               xmin = 5, xmax = 9,
               ymin = -0.6, ymax = -0.2,
               fill = "grey30", alpha = 0.15) +
      annotate("text",
               x = 7, y = -0.18,
               label = "Mackinawite / FeS\n(conceptual domain)",
               size = 3, colour = "grey10")
    
    # Pyrite / greigite-ish field
    p <- p +
      annotate("rect",
               xmin = 6, xmax = 9,
               ymin = -0.25, ymax = 0.15,
               fill = "grey60", alpha = 0.10) +
      annotate("text",
               x = 8, y = 0.17,
               label = "Pyrite / Fe₃S₄\n(conceptual domain)",
               size = 3, colour = "grey20")
  }
  
  p
}


# ---------------------------------------------------------
# 5. Plotting: kinetic Pourbaix
# ---------------------------------------------------------
plot_pourbaix_kin <- function(grid_df,
                              title = "Fe Pourbaix – kinetic overlay") {
  
  df <- grid_df %>%
    filter(is.finite(pH_eq), is.finite(Eh_V))
  
  p <- ggplot(df, aes(x = pH_eq, y = Eh_V)) +
    geom_tile(aes(fill = phase_kin)) +
    scale_fill_manual(
      values = fe_palette_kin,
      drop   = FALSE,
      name   = "Apparent dominant Fe pool"
    ) +
    labs(
      title = title,
      x     = "pH (equilibrium)",
      y     = "Eh (V, 25 °C)"
    ) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16)
    )
  
  # Fe2+/Fe3+ = 1 conceptual line
  vals <- df$log_Fe2_Fe3[is.finite(df$log_Fe2_Fe3)]
  if (length(vals) > 0 && min(vals) < 0 && max(vals) > 0) {
    p <- p +
      stat_contour(
        aes(z = log_Fe2_Fe3),
        breaks    = 0,
        colour    = "black",
        linewidth = 0.4
      )
  }
  
  # Kinetic critical Eh line
  Eh_fun <- function(x) 0.2 + 0.03 * (x - 7)   # Tunable
  p <- p + stat_function(
    fun      = Eh_fun,
    colour   = "black",
    linetype = "dashed",
    linewidth = 0.7
  ) +
    annotate("text",
             x = 4.3, y = Eh_fun(4.3) + 0.03,
             label = "Eh\u2091\u2093\u209c\u2099 (Fe(III) precipitation\nkinetically limited)",
             hjust = 0, size = 3.2)
  
  p
}


# ---------------------------------------------------------
# 6. Build oxic / anoxic grids and plots
# ---------------------------------------------------------

# Grid definition (moderate size to avoid memory issues)
pH_vals  <- seq(4, 9,  by = 0.25)  # 4–9
pe_oxic  <- seq(8, 17, by = 0.5)   # oxic soil-ish pe
pe_anox  <- seq(-3, 7, by = 0.5)   # anoxic microsite-ish pe

Fe_tot_val <- 1e-6   # dilute Fe, soil-relevant

# ---- Oxic grid: log fCO2 = -2 ----
grid_oxic <- run_grid(
  pH_vals  = pH_vals,
  pe_vals  = pe_oxic,
  Fe_tot   = Fe_tot_val,
  log_fCO2 = -2          # bulk oxic soil
)

cat("Oxic grid rows:", nrow(grid_oxic), "\n")
print(summary(grid_oxic$phase_eq))

# ---- Anoxic grid: log fCO2 = 0 ----
grid_anox <- run_grid(
  pH_vals  = pH_vals,
  pe_vals  = pe_anox,
  Fe_tot   = Fe_tot_val,
  log_fCO2 = 0           # anoxic microsite with high CO2
)

cat("Anoxic grid rows:", nrow(grid_anox), "\n")
print(summary(grid_anox$phase_eq))


# ---------------------------------------------------------
# 7. Make dual-panel (equilibrium + kinetic) figures
# ---------------------------------------------------------
p_oxic_eq <- plot_pourbaix_eq(
  grid_oxic,
  title = "Fe Pourbaix – Bulk Oxic Soil (equilibrium)\nlog fCO₂ = −2, Feᵗ = 10⁻⁶ M"
)

p_oxic_kin <- plot_pourbaix_kin(
  grid_oxic,
  title = "Fe Pourbaix – Bulk Oxic Soil (kinetic overlay)"
)

p_anox_eq <- plot_pourbaix_eq(
  grid_anox,
  title = "Fe Pourbaix – Anoxic Microsite (equilibrium)\nlog fCO₂ = 0, Feᵗ = 10⁻⁶ M"
)

p_anox_kin <- plot_pourbaix_kin(
  grid_anox,
  title = "Fe Pourbaix – Anoxic Microsite (kinetic overlay)"
)

# Dual-panel layouts
p_oxic_dual <- p_oxic_eq + p_oxic_kin + plot_layout(ncol = 2)
p_anox_dual <- p_anox_eq + p_anox_kin + plot_layout(ncol = 2)


print(p_oxic_dual)
print(p_anox_dual)


# ---------------------------------------------------------
# 8. Save publication-quality figures
# ---------------------------------------------------------
ggsave("Fe_Pourbaix_Oxic_equilibrium.png",
       p_oxic_eq, width = 7, height = 6, dpi = 600)
ggsave("Fe_Pourbaix_Oxic_kinetic.png",
       p_oxic_kin, width = 7, height = 6, dpi = 600)
ggsave("Fe_Pourbaix_Oxic_dual.png",
       p_oxic_dual, width = 12, height = 6, dpi = 600)

ggsave("Fe_Pourbaix_Anoxic_equilibrium.png",
       p_anox_eq, width = 7, height = 6, dpi = 600)
ggsave("Fe_Pourbaix_Anoxic_kinetic.png",
       p_anox_kin, width = 7, height = 6, dpi = 600)
ggsave("Fe_Pourbaix_Anoxic_dual.png",
       p_anox_dual, width = 12, height = 6, dpi = 600)

############################################################
# End of script
############################################################