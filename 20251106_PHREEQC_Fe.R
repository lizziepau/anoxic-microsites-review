# =============================================================================
# PHREEQC Fe-product grid modeling (phreeqc.dat)
# Robust parsing of Fe(II)/Fe(III) aqueous totals and Fe mineral stability
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(stringr)
  library(purrr)
  library(ggplot2)
})

# ---- PATHS ------------------------------------------------------------------
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/usr/local/share/doc/IPhreeqc/database/phreeqc.dat"

WORKDIR <- "/Users/epaulus/Documents/PHREEQC/ConceptModel"
dir.create(WORKDIR, showWarnings = FALSE, recursive = TRUE)

infile  <- file.path(WORKDIR, "fe_grid.pqi")
outfile <- file.path(WORKDIR, "fe_grid.out")

# ---- ENVIRONMENT GRID -------------------------------------------------------
grid_pH <- list(
  acidic        = c(4.6, 5.3, 5.9),
  circumneutral = c(6.1, 6.8, 7.4),
  alkaline      = c(7.6, 8.0, 8.5)
)

DOC_defs <- tibble(
  DOC_label = c("Low DOC", "Moderate DOC", "High DOC"),
  DOC_mgC_L = c(1, 5, 15)
)

redox_defs <- tibble(
  redox = c("Oxic", "Anoxic"),
  Eh_V  = c(0.50, -0.15)
)

ligand_defs <- tibble(
  ligands       = c("none", "carbonate", "sulfide", "caric+sulf"),
  add_carbonate = c(FALSE, TRUE, FALSE, TRUE),
  add_sulfide   = c(FALSE, FALSE, TRUE, TRUE)
)

# ---- CONSTANTS --------------------------------------------------------------
Fe2_mgL <- 2.5
Fe3_mgL <- 2.5
Na_mgL  <- 10
Cl_mgL  <- 10
CARB_mgC_L <- 20
SULF_mgS_L <- 1
Eh_to_pe <- function(Eh) Eh / 0.05916

# ---- INPUT BLOCK BUILDER ----------------------------------------------------
make_block <- function(id, pH, Eh, DOC_mgC_L, add_carbonate, add_sulfide) {
  pe <- Eh_to_pe(Eh)
  glue("
SOLUTION {id}
    temp 25
    pH {pH} charge
    pe {pe}
    units mg/L
    Na {Na_mgL}
    Cl {Cl_mgL}
    Fe(2) {Fe2_mgL}
    Fe(3) {Fe3_mgL}
{if (add_carbonate) glue('    C {CARB_mgC_L}') else ''}
{if (add_sulfide)   glue('    S {SULF_mgS_L}') else ''}

EQUILIBRIUM_PHASES {id}
    Goethite   0 0
    Hematite   0 0
    Siderite   0 0
    Pyrite     0 0
    Vivianite  0 0
END
")
}

# ---- BUILD PARAMETER GRID ---------------------------------------------------
grid <- expand_grid(
  pH_band = names(grid_pH),
  pH      = unlist(grid_pH, use.names = FALSE),
  DOC_label = DOC_defs$DOC_label,
  DOC_mgC_L = DOC_defs$DOC_mgC_L,
  redox = redox_defs$redox,
  Eh_V  = redox_defs$Eh_V,
  ligands = ligand_defs$ligands
) %>%
  left_join(ligand_defs, by = "ligands") %>%
  arrange(pH_band, DOC_label, redox, ligands, pH) %>%
  mutate(ID = row_number())

# ---- WRITE PHREEQC INPUT ----------------------------------------------------
cat("# PHREEQC Fe grid input\n\n", file = infile)
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

# ---- PARSE FE(II)/FE(III) TOTALS FROM SOLUTION COMPOSITION ------------------
lines <- readLines(outfile, warn = FALSE)
sol_all <- grep("Solution composition", lines, ignore.case = TRUE)
sol_start <- sol_all[seq(2, length(sol_all), by = 2)]
if (length(sol_start) == 0)
  stop("❌ Could not find any 'Solution composition' sections in .out file.")

extract_Fe <- function(idx) {
  block <- lines[idx:(idx + 200)]
  stop_line <- grep("Description of solution|^-+$", block)
  if (length(stop_line) > 0) block <- block[1:(stop_line[1]-1)]
  fe_lines <- grep("^\\s*Fe", block, value = TRUE)
  if (length(fe_lines) == 0) return(NULL)
  Fe2_val <- as.numeric(str_extract(fe_lines[grepl("Fe\\(2\\)", fe_lines)], "-?\\d\\.\\d+E[+-]\\d+"))
  Fe3_val <- as.numeric(str_extract(fe_lines[grepl("Fe\\(3\\)", fe_lines)], "-?\\d\\.\\d+E[+-]\\d+"))
  tibble(FeII_aq = sum(Fe2_val, na.rm = TRUE),
         FeIII_aq = sum(Fe3_val, na.rm = TRUE))
}

Fe_totals <- map_dfr(sol_start, extract_Fe) %>%
  mutate(Fe_total = FeII_aq + FeIII_aq,
         Fe_ratio = FeII_aq / pmax(Fe_total, 1e-30),
         log_FeII_FeIII = log10(pmax(FeII_aq, 1e-30) / pmax(FeIII_aq, 1e-30)))

Fe_totals <- bind_cols(Fe_totals,
                       grid %>% select(pH, Eh_V, DOC_mgC_L, DOC_label,
                                       add_carbonate, add_sulfide)) %>%
  mutate(Eh_state = if_else(Eh_V > 0, "Oxic", "Anoxic"))
write_csv(Fe_totals, file.path(WORKDIR, "Fe_totals_from_solution_composition.csv"))
message("✅ Parsed Fe(II)/Fe(III) totals.")

# ---- PARSE SATURATION INDICES (ROBUST) --------------------------------------
parse_si_block <- function(block, idx) {
  start <- grep("Saturation indices", block, ignore.case = TRUE)
  if (length(start) == 0) return(NULL)
  start_line <- start[1] + 4
  rel_end <- grep("^[-]{5,}|^\\s*$", block[start_line:length(block)])[1]
  end_line <- if (is.na(rel_end)) length(block) else start_line + rel_end - 2
  tbl <- block[start_line:end_line]
  tbl <- tbl[nzchar(tbl)]
  tbl <- tbl[!str_detect(tbl, "For a gas|phi|Batch|^-+$")]
  if (!length(tbl)) return(NULL)
  parsed <- map_dfr(tbl, function(line) {
    line <- str_squish(line)
    if (nchar(line) < 10) return(NULL)
    nums <- suppressWarnings(readr::parse_number(str_extract_all(line, "-?\\d+\\.?\\d*", simplify = TRUE)))
    if (length(nums) < 3) return(NULL)
    phase <- str_extract(line, "[A-Za-z0-9()_.:+-]+")
    formula <- str_extract(line, "[A-Za-z0-9()_.:+-]+$")
    tibble(Phase = phase, SI = nums[1], logIAP = nums[2], logK = nums[3],
           Formula = formula, ID = idx)
  })
  parsed %>% filter(!is.na(SI), !is.na(Phase), Phase != "")
}

lines <- readLines(outfile, warn = FALSE)
si_blocks <- split(lines, cumsum(grepl("End of simulation", lines, ignore.case = TRUE)))
parsed_SI <- map2_dfr(si_blocks, seq_along(si_blocks), parse_si_block)
message("✅ Parsed ", nrow(parsed_SI), " SI rows.")

# ---- FE MINERAL STABILITY & ΔSI ≥ 0.5 RULE ----------------------------------
Fe_phases <- c("Goethite", "Hematite", "Siderite",
               "Pyrite", "Mackinawite", "Fe(OH)2(s)", "Fe(OH)3(a)")

parsed_Fe <- parsed_SI %>%
  filter(Phase %in% Fe_phases) %>%
  group_by(ID) %>%
  arrange(desc(SI)) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 2) %>%
  mutate(
    ΔSI = SI - lead(SI),
    stable_phase = Phase[1],
    stable_SI = SI[1],
    runner_up = Phase[2],
    ΔSI = ifelse(is.na(ΔSI), NA, ΔSI),
    final_phase = case_when(
      !is.na(ΔSI) & ΔSI >= 0.5 ~ stable_phase,
      !is.na(ΔSI) & ΔSI < 0.5  ~ paste(stable_phase, runner_up, sep = " (tie with )"),
      TRUE ~ stable_phase
    )
  ) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup()

parsed_joined <- parsed_Fe %>%
  left_join(grid %>% select(ID, pH, Eh_V, DOC_mgC_L, DOC_label,
                            add_carbonate, add_sulfide),
            by = "ID")

out_stable <- file.path(WORKDIR, "stable_Fe_minerals_top_tiebreak.csv")
write_csv(parsed_joined, out_stable)
message("✅ Saved Fe mineral stability results to: ", out_stable)

# ---- VISUAL SUMMARY ---------------------------------------------------------
if (nrow(parsed_joined) > 0) {
  top_summary <- parsed_joined %>% count(final_phase, sort = TRUE)
  message("📊 Top Fe minerals predicted across grid:")
  print(top_summary)
  
  Fe_plot <- parsed_joined %>%
    ggplot(aes(x = pH, y = Eh_V, fill = final_phase)) +
    geom_tile(color = "white") +
    facet_wrap(~DOC_label, ncol = 1) +
    scale_fill_viridis_d() +
    theme_minimal(base_size = 12) +
    labs(title = "Most Stable Fe Mineral by pH, Eh, and DOC",
         x = "pH", y = "Eh (V)", fill = "Stable Phase")
  ggsave(file.path(WORKDIR, "Fe_mineral_stability_map.png"), Fe_plot,
         width = 8, height = 6, dpi = 300)
  message("✅ Saved Fe stability plot.")
}

message("✅ All analyses complete. Results in: ", WORKDIR)

