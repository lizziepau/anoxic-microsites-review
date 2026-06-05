## ============================================================
## Fe redox model with PHREEQC 3.8.6 (x86_64 under Rosetta)
## - CO2(g) equilibrium at log10 pCO2 = -3.5
## - Fe(II)/Fe(III) speciation
## - Ferrihydrite (Fe(OH)3(a)), Goethite, Siderite SIs
## - pH–pe grid and Eh–pH Fe predominance diagram
## ============================================================

## ---- PACKAGES ------------------------------------------------------------
library(glue)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

## ---- PATHS ---------------------------------------------------------------
## Adjust if necessary
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/Users/epaulus/Documents/LOAMS_Review/phreeqc/iphreeqc-3.8.6-17100/database/phreeqc.dat"

## ---- CORE PHREEQC WRAPPER -----------------------------------------------
run_phreeqc <- function(input_core,
                        phreeqc_exe = PHREEQC_EXE,
                        db_path     = DB_PATH,
                        workdir     = tempdir()) {
  
  infile  <- file.path(workdir, paste0("phreeqc_in_",  Sys.getpid(), ".pqi"))
  outfile <- file.path(workdir, paste0("phreeqc_out_", Sys.getpid(), ".txt"))
  selfile <- file.path(workdir, paste0("phreeqc_sel_", Sys.getpid(), ".txt"))
  
  ## USER_PUNCH only gives the variables we care about
  ## SELECTED_OUTPUT -reset true clears default columns (sim, state, etc.)
  input_full <- glue("
USER_PUNCH
    -headings pH Fe_tot Fe2 Fe3 SI_FeOH3 SI_Goethite SI_Siderite
 10 PUNCH -LA(\"H+\")
 20 PUNCH TOT(\"Fe\")
 21 PUNCH MOL(\"Fe+2\")
 22 PUNCH MOL(\"Fe+3\")
 23 PUNCH SI(\"Fe(OH)3(a)\")
 24 PUNCH SI(\"Goethite\")
 25 PUNCH SI(\"Siderite\")
END

SELECTED_OUTPUT
    -file {selfile}
    -reset true
    -user_punch true
    -high_precision true
END

{input_core}
")
  
  # Write input
  writeLines(input_full, infile)
  
  # Run PHREEQC silently
  suppressWarnings(
    system2(phreeqc_exe, args = c(infile, outfile, db_path), stdout = NULL, stderr = NULL)
  )
  
  # Read output silently and strip blank trailing columns
  df <- suppressWarnings(
    readr::read_table(selfile, comment = "#", show_col_types = FALSE)
  ) %>%
    select(-matches("^X[0-9]+$"))  # remove phantom columns silently
  
  df
}

## ---- SAFE SINGLE-RUN WRAPPER --------------------------------------------
## pH, pe in bulk, Fe_tot in mol/kgw
## use_CO2 = TRUE => equilibrate with CO2(g) at log10 pCO2 = -3.5
safe_run_sim <- function(pH,
                         pe,
                         Fe_tot = 1e-3,
                         use_CO2 = TRUE) {
  tryCatch({
    eq_block <- if (use_CO2) {
      "EQUILIBRIUM_PHASES 1
    CO2(g) -3.5
END"
    } else {
      "END"
    }
    
    input_core <- glue("
SOLUTION 1
    temp 25
    pH {pH}
    pe {pe}
    units mol/kgw
    Na 0.1
    Cl 0.1
    Fe {Fe_tot}

{eq_block}
")
    run_phreeqc(input_core)
  },
  error = function(e) {
    ## On failure, return a single NA row with the expected columns
    tibble(
      pH          = NA_real_,
      Fe_tot      = NA_real_,
      Fe2         = NA_real_,
      Fe3         = NA_real_,
      SI_FeOH3    = NA_real_,
      SI_Goethite = NA_real_,
      SI_Siderite = NA_real_
    )
  })
}

## =======================================================================
## 1) 1D pH SLICE at fixed pe (e.g., bulk oxic profile at pe = 4)
## =======================================================================

pH_seq_1D <- seq(4, 7, by = 0.25)
pe_fixed  <- 4

grid_1D_raw <- tibble(pH_input = pH_seq_1D) %>%
  mutate(
    res = map(pH_input, ~ safe_run_sim(pH = .x, pe = pe_fixed, Fe_tot = 1e-3, use_CO2 = TRUE))
  ) %>%
  unnest(res, names_sep = "_")

grid_1D <- grid_1D_raw %>%
  mutate(
    Fe_tot = as.numeric(res_Fe_tot),
    Fe2    = as.numeric(res_Fe2),
    Fe3    = as.numeric(res_Fe3)
  )

## Long format for Fe(II)/Fe(III) vs pH
grid_1D_long <- grid_1D %>%
  transmute(
    pH = pH_input,
    Fe2 = Fe2,
    Fe3 = Fe3
  ) %>%
  pivot_longer(
    cols      = c(Fe2, Fe3),
    names_to  = "species",
    values_to = "molality"
  )

## Example plot: Fe(II) & Fe(III) vs pH (log scale)
p_fe_1D <- ggplot(grid_1D_long, aes(x = pH, y = molality, color = species)) +
  geom_line(linewidth = 1.0) +
  scale_y_log10() +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "Molality (mol/kgw)",
    title = glue("Fe(II) and Fe(III) vs pH at pe = {pe_fixed} (CO2(g) eq)")
  )

## Example plot: Siderite SI vs pH
grid_1D_siderite <- grid_1D_raw %>%
  mutate(SI_Siderite = as.numeric(res_SI_Siderite))

p_siderite_1D <- ggplot(grid_1D_siderite,
                        aes(x = pH_input, y = SI_Siderite)) +
  geom_line(linewidth = 1.0) +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "pH (input)",
    y = "SI (Siderite)",
    title = glue("Siderite saturation vs pH at pe = {pe_fixed} (CO2(g) eq)")
  )

## =======================================================================
## 2) 2D pH–pe GRID: Fe redox, mineral SIs, Eh, Fe(II)/Fe(III) ratio
## =======================================================================

pH_vals <- seq(4, 9,  by = 0.5)
pe_vals <- seq(-1, 10, by = 0.5)   ## Keep moderately bounded to avoid crazy nonconvergence

param_grid <- tidyr::crossing(
  pH_input = pH_vals,
  pe_input = pe_vals
)

grid_raw <- param_grid %>%
  mutate(
    res = map2(
      pH_input, pe_input,
      ~ safe_run_sim(pH = .x, pe = .y, Fe_tot = 1e-3, use_CO2 = TRUE)
    )
  ) %>%
  unnest(res, names_sep = "_")

grid_redox <- grid_raw %>%
  mutate(
    Fe_tot      = as.numeric(res_Fe_tot),
    Fe2         = as.numeric(res_Fe2),
    Fe3         = as.numeric(res_Fe3),
    SI_FeOH3    = as.numeric(res_SI_FeOH3),
    SI_Goethite = as.numeric(res_SI_Goethite),
    SI_Siderite = as.numeric(res_SI_Siderite),
    Eh_V        = 0.05916 * pe_input,                ## Eh (V) at 25 °C
    FeII_frac   = Fe2 / Fe_tot,
    log_FeII_FeIII = log10(Fe2 / Fe3)
  ) %>%
  filter(!is.na(Fe_tot)) %>%        ## drop failed sims
  filter(is.finite(log_FeII_FeIII)) ## drop extreme Fe3 ~ 0 cases for plotting ratio

## -----------------------------------------------------------------------
## 2a) Siderite stability: SI contours in pH–pe space
## -----------------------------------------------------------------------
p_siderite_2D <- ggplot(
  grid_redox,
  aes(x = pH_input, y = pe_input, z = SI_Siderite)
) +
  stat_contour(
    breaks    = c(-1, 0, 1),
    linewidth = 0.8
  ) +
  geom_contour(
    breaks = 0,
    linewidth = 1.2,
    linetype = "solid"
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "pe (input)",
    title = "Siderite saturation contours (SI = -1, 0, +1)"
  )

## -----------------------------------------------------------------------
## 2b) Goethite stability field & Ferrihydrite boundary
## -----------------------------------------------------------------------

## Goethite field (SI_Goethite >= 0) as filled, ferrihydrite (Fe(OH)3(a)) SI=0 line
p_goethite_field <- ggplot(
  grid_redox,
  aes(x = pH_input, y = pe_input)
) +
  geom_raster(aes(fill = SI_Goethite), na.rm = TRUE, interpolate = FALSE) +
  stat_contour(
    aes(z = SI_Goethite),
    breaks    = 0,
    linewidth = 1.0,
    color     = "black"
  ) +
  scale_fill_gradient2(
    midpoint = 0,
    name     = "SI(Goethite)"
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "pe (input)",
    title = "Goethite stability field (SI = 0 contour in black)"
  )

## Ferrihydrite boundary (Fe(OH)3(a) ~ ferrihydrite proxy)
p_ferrihydrite_boundary <- ggplot(
  grid_redox,
  aes(x = pH_input, y = pe_input)
) +
  stat_contour(
    aes(z = SI_FeOH3),
    breaks    = 0,
    linewidth = 1.2
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "pe (input)",
    title = "Ferrihydrite (Fe(OH)3(a)) SI = 0 boundary"
  )

## -----------------------------------------------------------------------
## 2c) Fe(II)/Fe(III) ratio contours in pH–pe space
## -----------------------------------------------------------------------

## Use log10(Fe2/Fe3) as z; log10 = 0 => Fe2 = Fe3
p_fe_ratio <- ggplot(
  grid_redox,
  aes(x = pH_input, y = pe_input, z = log_FeII_FeIII)
) +
  stat_contour(
    breaks    = seq(-4, 4, by = 1),
    linewidth = 0.8
  ) +
  stat_contour(
    breaks    = 0,   ## Fe2 = Fe3 line
    linewidth = 1.3,
    linetype  = "solid"
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "pe (input)",
    title = "log10(Fe(II)/Fe(III)) contours",
    subtitle = "Thick line: Fe(II) = Fe(III)"
  )

## -----------------------------------------------------------------------
## 2d) Eh–pH Fe predominance diagram (very simple phase rules)
## -----------------------------------------------------------------------

grid_phase <- grid_redox %>%
  mutate(
    phase = case_when(
      SI_Siderite > 0               ~ "Siderite",
      SI_FeOH3   > 0               ~ "Ferrihydrite",
      SI_Goethite > 0              ~ "Goethite",
      Fe2 > Fe3                    ~ "Fe(II)_aq",
      Fe3 >= Fe2                   ~ "Fe(III)_aq",
      TRUE                         ~ "Unclassified"
    ),
    phase = factor(
      phase,
      levels = c("Fe(II)_aq", "Fe(III)_aq", "Siderite",
                 "Ferrihydrite", "Goethite", "Unclassified")
    )
  )

p_pourbaix <- ggplot(
  grid_phase,
  aes(x = pH_input, y = Eh_V, fill = phase)
) +
  geom_tile() +
  theme_bw(base_size = 14) +
  labs(
    x = "pH (input)",
    y = "Eh (V)",
    title = "Simplified Eh–pH Fe predominance diagram",
    subtitle = "Aqueous Fe(II)/Fe(III) vs Fe minerals (CO2(g) eq)"
  )

## ============================================================
## Pourbaix:
print(p_pourbaix)
## ============================================================

#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
#========================================================================================================================
setwd("/Users/epaulus/Documents/LOAMS_Review/Reactions")
getwd()

################################################################################
# CLEAN WORKING FE-ONLY POURBAIX SCRIPT (NO _res COLUMNS)
################################################################################

library(glue)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/Users/epaulus/Documents/LOAMS_Review/phreeqc/iphreeqc-3.8.6-17100/database/phreeqc.dat"

# ------------------------------------------------------------------------------
# 1) PHREEQC RUNNER (must match actual PHREEQC column names)
# ------------------------------------------------------------------------------
run_sim_fe <- function(pH_in, pe_in,
                       Fe_tot   = 1e-4,
                       log_fCO2 = -2,
                       phreeqc_exe = PHREEQC_EXE,
                       db_path     = DB_PATH) {
  
  wdir <- tempdir()
  infile  <- file.path(wdir, paste0("fe_in_",  Sys.getpid(), ".pqi"))
  outfile <- file.path(wdir, paste0("fe_out_", Sys.getpid(), ".out"))
  selfile <- file.path(wdir, paste0("fe_sel_", Sys.getpid(), ".txt"))
  
  input <- glue("
USER_PUNCH
  -headings pH_eq pe_eq Fe2 Fe3 FeOH FeOH2 FeOH3 SI_fh SI_go SI_sid
10 PUNCH -LA(\"H+\")
20 PUNCH -LA(\"e-\")
30 PUNCH MOL(\"Fe+2\")
40 PUNCH MOL(\"Fe+3\")
50 PUNCH MOL(\"FeOH+\")
60 PUNCH MOL(\"Fe(OH)2\")
70 PUNCH MOL(\"Fe(OH)3(aq)\")
80 PUNCH SI(\"Ferrihydrite\")
#90 PUNCH SI(\"Goethite\")
100 PUNCH SI(\"Siderite\")
END

SELECTED_OUTPUT
  -file {selfile}
  -reset true
  -user_punch true
END

SOLUTION 1
  temp 25
  pH {pH_in}
  pe {pe_in}
  units mol/kgw
  Na 0.1
  Cl 0.1
  Fe {Fe_tot}
END

EQUILIBRIUM_PHASES 1
  CO2(g) {log_fCO2}
")
  
  writeLines(input, infile)
  
  system2(phreeqc_exe, args = c(infile, outfile, db_path),
          stdout = FALSE, stderr = FALSE)
  
  if (!file.exists(selfile)) return(NULL)
  
  df <- suppressWarnings(read_table(selfile, comment = "#", show_col_types = FALSE))
  if (nrow(df) == 0) return(NULL)
  
  df %>% slice(n()) %>% mutate(pH_in = pH_in, pe_in = pe_in)
}

# ------------------------------------------------------------------------------
# 2) GRID RUNNER — NOTE: using actual column names ("pH_eq", not "pH_eq_res")
# ------------------------------------------------------------------------------
run_grid <- function(pH_vals, pe_vals,
                     Fe_tot   = 1e-4,
                     log_fCO2 = -2) {
  
  expand_grid(pH_in = pH_vals,
              pe_in = pe_vals) %>%
    
    mutate(res = purrr::map2(
      pH_in, pe_in,
      ~ run_sim_fe(.x, .y, Fe_tot = Fe_tot, log_fCO2 = log_fCO2)
    )) %>%
    
    filter(!purrr::map_lgl(res, is.null)) %>%
    
    tidyr::unnest(res, names_repair = "unique") %>%
    
    mutate(
      Eh_V = 0.05916 * pe_eq,
      
      Fe_aq = Fe2 + Fe3 + FeOH + FeOH2 + FeOH3,
      
      Fe2_frac = Fe2 / pmax(Fe_aq, 1e-30),
      Fe3_frac = Fe3 / pmax(Fe_aq, 1e-30),
      
      log_Fe2_Fe3 = log10(
        pmax(Fe2, 1e-30) / pmax(Fe3, 1e-30)
      ),
      
      phase = case_when(
        SI_sid >= 0 ~ "Siderite",
        #SI_go  >= 0 ~ "Goethite",
        SI_fh  >= 0 ~ "Ferrihydrite",
        Fe2_frac > 0.9 ~ "Fe2+ (aq)",
        Fe3_frac > 0.9 ~ "Fe3+ (aq)",
        TRUE           ~ "Mixed Fe"
      )
    )
}

# ------------------------------------------------------------------------------
# 3) KINETIC LINE
# ------------------------------------------------------------------------------
Eh_crit_fun <- function(pH) 0.55 + 0.03 * (pH - 7)   # conceptual

# ------------------------------------------------------------------------------
# 4) PLOT FUNCTION
# ------------------------------------------------------------------------------
plot_pourbaix <- function(grid_df, title = "") {
  
  pH_seq <- seq(min(grid_df$pH_eq), max(grid_df$pH_eq), length.out = 200)
  kin_df <- data.frame(
    pH_eq = pH_seq,
    Eh_crit = Eh_crit_fun(pH_seq)
  )
  
  ggplot(grid_df, aes(pH_eq, Eh_V)) +
    geom_raster(aes(fill = phase)) +
    scale_fill_manual(values = c(
      "Fe2+ (aq)"    = "#1b9e77",
      "Mixed Fe"     = "#7570b3",
      "Fe3+ (aq)"    = "#d95f02",
      "Ferrihydrite" = "#e6ab02",
      #"Goethite"     = "#a6761d",
      "Siderite"     = "#66a61e"
    )) +
    geom_line(data = kin_df, aes(pH_eq, Eh_crit),
              inherit.aes = FALSE,
              color = "black",
              linewidth = 0.7,
              linetype = "longdash") +
    annotate("text",
             x = mean(pH_seq),
             y = Eh_crit_fun(7) + 0.05,
             label = "Kinetic limit\nFe(III) precipitation slow",
             size = 3.5) +
    labs(title = title,
         x = "pH (equilibrium)",
         y = "Eh (V, 25°C)") +
    theme_bw(15) +
    theme(panel.grid = element_blank())
}

# ------------------------------------------------------------------------------
# 5) BUILD OXIC + ANOXIC GRIDS
# ------------------------------------------------------------------------------
pH_vals  <- seq(4, 9, 0.25)
pe_oxic  <- seq(8, 17, 0.5)
pe_anox  <- seq(-2, 6, 0.5)

test <- run_sim_fe(7, 12)
names(test)

grid_oxic <- run_grid(pH_vals, pe_oxic, Fe_tot = 1e-4, log_fCO2 = -2)
grid_anox <- run_grid(pH_vals, pe_anox, Fe_tot = 1e-4, log_fCO2 = 0)

p_oxic <- plot_pourbaix(grid_oxic, "Fe Pourbaix — Bulk Oxic Soil")
p_anox <- plot_pourbaix(grid_anox, "Fe Pourbaix — Anoxic Microsite")

print(p_oxic)
print(p_anox)
