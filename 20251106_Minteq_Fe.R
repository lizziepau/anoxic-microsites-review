suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(stringr)
})

# ==== PATHS =====================================================================
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/usr/local/share/doc/IPhreeqc/database/minteq.v4.dat"
WORKDIR     <- "/Users/epaulus/Documents/PHREEQC/ConceptModel"
dir.create(WORKDIR, showWarnings = FALSE, recursive = TRUE)

infile   <- file.path(WORKDIR, "fe_speciation_2Eh.pqi")
outfile  <- file.path(WORKDIR, "fe_speciation_2Eh.out")
sel_file <- file.path(WORKDIR, "selected_si.txt")

# ==== GRID ======================================================================
grid_pH <- list(
  acidic        = c(4.6, 5.3, 5.9),
  circumneutral = c(6.1, 6.8, 7.4),
  alkaline      = c(7.6, 8.0, 8.5)
)

DOC_defs <- tibble(
  DOC_label = c("Low DOC","Moderate DOC","High DOC"),
  DOC_mgC_L = c(1,5,15)
)

ligand_defs <- tibble(
  ligands        = c("none","carbonate","sulfide","carb+sulf"),
  add_carbonate  = c(FALSE, TRUE,  FALSE, TRUE),
  add_sulfide    = c(FALSE, FALSE, TRUE,  TRUE)
)

Eh_defs <- tibble(redox = c("Anoxic","Oxic"),
                  Eh_V  = c(-0.20, +0.40))

# ==== CONSTANTS =================================================================
units_label   <- "mmol/kgw"
Fe_total_mmol <- 1e-5
Na_bg_mmol    <- 1e-2
Cl_bg_mmol    <- 1e-2
Alk_base      <- 0.1
HS_trace      <- 1e-6

fe_phases  <- c("Goethite","Hematite","Siderite","Pyrite","Vivianite")
fe_species <- c("Fe+2","Fe+3","FeOH+","Fe(OH)2","Fe(OH)3-","Fe(OH)4-",
                "FeHCO3+","FeSO4","FeCl2+")

message("Using Fe phases: ", paste(fe_phases, collapse = ", "))

grid <- expand_grid(
  pH_band   = names(grid_pH),
  pH        = unlist(grid_pH, use.names = FALSE),
  DOC_label = DOC_defs$DOC_label,
  DOC_mgC_L = DOC_defs$DOC_mgC_L,
  redox     = Eh_defs$redox,
  Eh_V      = Eh_defs$Eh_V,
  ligands   = ligand_defs$ligands
) %>%
  left_join(ligand_defs, by = "ligands") %>%
  arrange(pH_band, DOC_label, redox, ligands, pH) %>%
  mutate(ID = row_number())

# ==== INPUT BUILDER =============================================================
write_selected_output_block <- function(path) cat("", file = infile)

make_block <- function(id, pH, Eh, add_carbonate, add_sulfide, DOC_mgC_L) {
  Fe_line  <- glue("  Fe {1e-6}")        # even smaller Fe
  add_C4   <- if (isTRUE(add_carbonate)) glue("  C(4)  {format(Alk_base * 1e-3, scientific=TRUE)}") else ""
  add_Sm2  <- if (isTRUE(add_sulfide))   glue("  S(-2) {format(HS_trace, scientific=TRUE)}") else ""
  
  redox_buf <- if (Eh < 0) "  O2(g) -55 0" else "  O2(g) -1 0"
  fe_eq     <- paste(sprintf("  %s 0 0", fe_phases), collapse = "\n")
  lig_label <- if (isTRUE(add_carbonate) && isTRUE(add_sulfide)) "carb+sulf"
  else if (isTRUE(add_carbonate)) "carbonate"
  else if (isTRUE(add_sulfide)) "sulfide" else "none"
  
  glue(
    'SOLUTION {id}
  temp 25
  pH {pH}
  pe {if (Eh < 0) -2 else 4}
  units {units_label}
  water 1
  Na {Na_bg_mmol}
  Cl {Cl_bg_mmol}
{Fe_line}
{add_C4}
{add_Sm2}
KNOBS
  -iterations 50000
  -tolerance 1e-1
  -convergence_tolerance 1e-2
  -warnings off
SAVE solution {id}

USE solution {id}
EQUILIBRIUM_PHASES {id}
{redox_buf}
{fe_eq}

SELECTED_OUTPUT
  -file "{sel_file}"
  -reset false
  -high_precision true
  -simulation true
  -pH true
  -pe true
  -si {paste(fe_phases, collapse=" ")}
  -totals Fe Fe(2) Fe(3) Na Cl C(4) S(-2) Alkalinity
  -molalities {paste(fe_species, collapse=" ")}

USER_PUNCH
  -headings ID pH_set Eh_tag DOC_mgC_L ligands carbonate sulfide
  -start
  10 PUNCH {id}, {pH}, "{if (Eh<0) "anoxic" else "oxic"}", {DOC_mgC_L}, "{lig_label}", {as.integer(isTRUE(add_carbonate))}, {as.integer(isTRUE(add_sulfide))}
  -end
END

'
  )
}

# ==== RUNNER ====================================================================
run_phreeqc <- function() {
  if (file.exists(infile)) file.remove(infile)
  if (file.exists(sel_file)) file.remove(sel_file)
  write_selected_output_block(sel_file)
  glue('"{PHREEQC_EXE}" "{infile}" "{outfile}" "{DB_PATH}"')
}

# ==== SMOKE TEST ================================================================
cmd <- run_phreeqc()
cat(make_block(1, 6.8, 0.4, FALSE, FALSE, 1), file = infile, append = TRUE)
message("Running smoke test: ", cmd)
status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
message("PHREEQC exit code: ", status)

if (file.exists(sel_file)) {
  message("✅ Smoke test wrote: ", sel_file)
  sel_raw <- readr::read_table(sel_file, show_col_types = FALSE)
  print(head(sel_raw))
} else {
  message("⚠️ Output file not found; check tolerance or iteration settings.")
}

# ==== FULL GRID RUN =============================================================
cmd <- run_phreeqc()
walk(seq_len(nrow(grid)), \(i)
     cat(make_block(grid$ID[i], grid$pH[i], grid$Eh_V[i],
                    grid$add_carbonate[i], grid$add_sulfide[i], grid$DOC_mgC_L[i]),
         file = infile, append = TRUE))
message("Running full grid: ", cmd)
status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
message("PHREEQC exit code: ", status)

if (!file.exists(sel_file))
  stop("❌ Full grid did not produce selected_si.txt. Check .out tail.")

# ==== PARSE OUTPUT ==============================================================
sel_raw <- readr::read_table(sel_file, show_col_types = FALSE)
fe_cols <- intersect(fe_species, names(sel_raw))
spec_long <- sel_raw |>
  select(simulation, pH, pe, all_of(fe_cols)) |>
  pivot_longer(cols = all_of(fe_cols), names_to="species", values_to="molality")

FeII_set  <- c("Fe+2","FeOH+","Fe(OH)2","FeHCO3+","FeSO4","FeCl2+")
FeIII_set <- c("Fe+3","Fe(OH)3-","Fe(OH)4-")

spec_summary <- spec_long |>
  mutate(redox_state = case_when(
    species %in% FeII_set  ~ "Fe(II)",
    species %in% FeIII_set ~ "Fe(III)",
    TRUE ~ "other"
  )) |>
  group_by(simulation, pH, pe, redox_state) |>
  summarise(molality=sum(molality,na.rm=TRUE),.groups="drop") |>
  group_by(simulation, pH, pe) |>
  mutate(frac = molality / sum(molality, na.rm=TRUE)) |>
  ungroup()

out_csv_sel <- file.path(WORKDIR, "selected_output_all.csv")
out_csv_sum <- file.path(WORKDIR, "fe_redox_fraction_summary.csv")
readr::write_csv(sel_raw, out_csv_sel)
readr::write_csv(spec_summary, out_csv_sum)
message("✅ Wrote: ", out_csv_sel)
message("✅ Wrote: ", out_csv_sum)

