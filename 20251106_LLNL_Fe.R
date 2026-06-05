# =============================================================================
# PHREEQC Fe-product grid modeling (LLNL database)
# Stable setup: pH fixed with 'charge', pe from Eh, TOTAL Fe
# Carbonate/sulfide as tiny traces (no alkalinity totals, no element 'charge')
# ΔSI ≥ 0.5 => single winner; else list top-2
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse); library(glue); library(stringr)
  library(purrr); library(ggplot2)
})

# ---- PATHS ------------------------------------------------------------------
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/usr/local/share/doc/IPhreeqc/database/llnl.dat"
WORKDIR     <- "/Users/epaulus/Documents/PHREEQC/ConceptModel"
dir.create(WORKDIR, showWarnings = FALSE, recursive = TRUE)
infile  <- file.path(WORKDIR, "fe_grid.pqi")
outfile <- file.path(WORKDIR, "fe_grid.out")

# ---- GRID -------------------------------------------------------------------
grid_pH <- list(
  acidic        = c(4.6, 5.3, 5.9),
  circumneutral = c(6.1, 6.8, 7.4),
  alkaline      = c(7.6, 8.0, 8.5)
)

DOC_defs <- tibble(
  DOC_label = c("Low DOC", "Moderate DOC", "High DOC"),
  DOC_mgC_L = c(1, 5, 15)
)

ligand_defs <- tibble(
  ligands       = c("none", "carbonate", "sulfide", "carb+sulf"),
  add_carbonate = c(FALSE, TRUE, FALSE, TRUE),
  add_sulfide   = c(FALSE, FALSE, TRUE, TRUE)
)

Eh_values <- c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5)

# ---- CONSTANTS --------------------------------------------------------------
units_label <- "mmol/kgw"
Eh_to_pe  <- function(Eh) Eh / 0.05916

# Keep Fe moderate to avoid violent first-iteration precipitation
Fe_total_mmol <- 0.03     # ≈ 1.7 mg/L
Na_bg_mmol    <- 1.0
Cl_bg_mmol    <- 1.0

# Ligands as tiny traces (present/absent toggles without constraining charge)
HCO3_trace <- 1e-4   # 0.1 µmol/kgw
HS_trace   <- 1e-5   # 0.01 µmol/kgw

# ---- INPUT BLOCK BUILDER ----------------------------------------------------
make_block <- function(id, pH, Eh, DOC_mgC_L, add_carbonate, add_sulfide) {
  pe <- Eh_to_pe(Eh); pe <- max(min(pe, 40), -40)
  
  add_HCO3 <- if (add_carbonate) glue("    HCO3- {format(HCO3_trace, scientific = TRUE)}") else ""
  add_HS   <- if (add_sulfide)   glue("    HS-   {format(HS_trace,   scientific = TRUE)}") else ""
  
  glue("
SOLUTION {id}
    temp 25
    pH {pH} charge
    pe {pe}
    units {units_label}
    water 1
    Na {Na_bg_mmol}
    Cl {Cl_bg_mmol}
    Fe {Fe_total_mmol}
{add_HCO3}
{add_HS}

KNOBS
    -iterations 400
    -tolerance 1e-12
    -step_size 1.0

EQUILIBRIUM_PHASES {id}
    Goethite    0 0
    Hematite    0 0
    Siderite    0 0
    Pyrite      0 0
    Vivianite   0 0
END
")
}

# ---- PARAMETER GRID ---------------------------------------------------------
grid <- expand_grid(
  pH_band   = names(grid_pH),
  pH        = unlist(grid_pH, use.names = FALSE),
  DOC_label = DOC_defs$DOC_label,
  DOC_mgC_L = DOC_defs$DOC_mgC_L,
  Eh_V      = Eh_values,
  ligands   = ligand_defs$ligands
) %>%
  left_join(ligand_defs, by = "ligands") %>%
  arrange(pH_band, DOC_label, Eh_V, ligands, pH) %>%
  mutate(ID = row_number())

# ---- WRITE INPUT ------------------------------------------------------------
cat("# PHREEQC Fe grid input (LLNL; stable: pH charge, pe, total Fe, trace ligands)\n\n", file = infile)
for (i in seq_len(nrow(grid))) {
  block <- make_block(
    grid$ID[i], grid$pH[i], grid$Eh_V[i],
    grid$DOC_mgC_L[i], grid$add_carbonate[i], grid$add_sulfide[i]
  )
  cat(trimws(block), "\n\n", file = infile, append = TRUE)
}
message("✅ Wrote PHREEQC input: ", infile)

# ---- RUN PHREEQC ------------------------------------------------------------
cmd <- glue('"{PHREEQC_EXE}" "{infile}" "{outfile}" "{DB_PATH}"')
message("Running: ", cmd)
status <- system(cmd)
if (status != 0) stop("❌ PHREEQC run failed; check .out file for details.")
message("✅ PHREEQC run completed: ", outfile)

# ---- PARSE SATURATION INDICES ----------------------------------------------
lines <- readLines(outfile, warn = FALSE)
si_heads <- grep("^[-]+Saturation indices[-]+", lines)
if (!length(si_heads)) stop("❌ No 'Saturation indices' sections found in .out")

# Split file at 'End of simulation' to keep ID alignment
blocks <- split(lines, cumsum(grepl("^[-]+\\s*End of simulation", lines)))

parse_si_block <- function(block, id) {
  s <- grep("^[-]+Saturation indices[-]+", block)
  if (!length(s)) return(NULL)
  s <- s[1] + 3
  rel_end <- grep("^\\s*$|^\\*\\*|^[-]{5,}", block[s:length(block)])[1]
  e <- if (is.na(rel_end)) length(block) else s + rel_end - 2
  tbl <- block[s:e]
  tbl <- tbl[nzchar(str_trim(tbl))]
  tbl <- tbl[!str_detect(tbl, "For a gas|Fugacity|phi")]
  if (!length(tbl)) return(NULL)
  
  map_dfr(tbl, function(z) {
    z <- str_squish(z)
    parts <- str_split_fixed(z, "\\s+", 5)
    if (ncol(parts) < 4) return(NULL)
    tibble(
      Phase   = parts[,1],
      SI      = suppressWarnings(as.numeric(parts[,2])),
      logIAP  = suppressWarnings(as.numeric(parts[,3])),
      logK    = suppressWarnings(as.numeric(parts[,4])),
      Formula = if (ncol(parts) >= 5) parts[,5] else NA_character_
    )
  }) %>% mutate(ID = id) %>% filter(!is.na(SI))
}

parsed_SI <- map2_dfr(blocks, seq_along(blocks), parse_si_block)
message("✅ Parsed ", nrow(parsed_SI), " SI rows.")

# ---- SELECT Fe MINERALS & APPLY TIE-BREAK -----------------------------------
Fe_keep <- c("Goethite","Hematite","Siderite","Pyrite","Vivianite")

stable_top <- parsed_SI %>%
  filter(Phase %in% Fe_keep, SI > 0) %>%
  left_join(grid %>% select(ID, pH, Eh_V, DOC_label, add_carbonate, add_sulfide),
            by = "ID") %>%
  group_by(pH, Eh_V, DOC_label, add_carbonate, add_sulfide) %>%
  arrange(desc(SI), desc(logK)) %>%
  slice_head(n = 2) %>%
  mutate(delta_SI = if (n() >= 2) SI[1] - SI[2] else NA_real_,
         final_phase = ifelse(!is.na(delta_SI) & delta_SI >= 0.5,
                              Phase[1],
                              paste(Phase, collapse = ", "))) %>%
  ungroup()

write_csv(stable_top, file.path(WORKDIR, "stable_Fe_minerals_top_tiebreak.csv"))
message("✅ Saved stable phase summary.")

# ---- PLOT -------------------------------------------------------------------
if (nrow(stable_top) > 0) {
  stable_top <- stable_top %>%
    mutate(Eh_state = ifelse(Eh_V >= 0, "Oxic", "Anoxic"))
  
  message("📊 Top Fe minerals predicted across grid:")
  print(stable_top %>% count(final_phase, sort = TRUE))
  
  Fe_plot <- ggplot(stable_top, aes(x = pH, y = Eh_V, fill = final_phase)) +
    geom_tile(color = "white") +
    facet_grid(Eh_state ~ DOC_label) +
    scale_fill_viridis_d(option = "plasma") +
    theme_minimal(base_size = 12) +
    labs(title = "Most Stable Fe Mineral by pH, Eh, and DOC (LLNL; ΔSI ≥ 0.5)",
         x = "pH", y = "Eh (V)", fill = "Stable Phase")
  
  ggsave(file.path(WORKDIR, "Fe_mineral_stability_map_expandedEh.png"),
         Fe_plot, width = 9, height = 7, dpi = 300)
  message("✅ Saved Fe stability plot.")
} else {
  warning("No Fe phases with SI>0 found.")
}

message("✅ All analyses complete. Results in: ", WORKDIR)