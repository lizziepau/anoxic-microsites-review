# ============================================================
#  MacPHREEQC Fe Redox Grid — Final Unified Pipeline
#  Author: Lizzie Paulus (2025-11-08)
#  Runs inline in R, ensures absolute output paths, parses results.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(fs)
  library(stringr)
  library(purrr)
})

# ---- 1. PATHS ------------------------------------------------------------
BASE_DIR <- path.expand("~/Documents/PHREEQC/ConceptModel")
WORKDIR  <- file.path(BASE_DIR, "Fe_redox_grid")
OUTPUTS  <- file.path(WORKDIR, "outputs")
DB_PATH  <- "/Applications/MacPHREEQC.app/Contents/Resources/database/minteq.v4.dat"
PHREEQC_EXE <- "/Applications/MacPHREEQC.app/Contents/MacOS/phreeqc"

dir_create(OUTPUTS, recurse = TRUE)
cat(glue("📂 Working directory: {WORKDIR}\n📤 Outputs: {OUTPUTS}\n📘 Database: {DB_PATH}\n\n"))

# ---- 2. DEFINE GRID ------------------------------------------------------
regimes <- tibble(
  regime = c("acidic", "neutral", "alkaline"),
  pH     = c(5.0, 7.0, 8.2)
)

DOC_grid <- tibble(
  DOC_label = c("No_DOC", "Low_DOC", "Moderate_DOC", "High_DOC"),
  DOC_value = c(0, 2e-5, 8e-5, 1e-4)
)

sulfide_opts    <- c("noSulfide", "withSulfide")
carbonate_opts  <- c("noCarbonate", "withCarbonate")
transition_opts <- c("Anoxic_to_Oxic", "Oxic_to_Anoxic")

grid <- tidyr::crossing(
  regimes,
  sulfide    = sulfide_opts,
  carbonate  = carbonate_opts,
  DOC_grid,
  transition = transition_opts
)

# ---- 3. PQI WRITER (absolute SELECTED_OUTPUT paths) ----------------------
write_pqi <- function(regime, pH, sulfide, carbonate, DOC_label, DOC_value, transition) {
  dir_create(OUTPUTS, recurse = TRUE)
  fname  <- glue("fe_{regime}__{sulfide}__{carbonate}__{DOC_label}__{transition}.pqi")
  fpath  <- file.path(WORKDIR, fname)
  selout <- file.path(OUTPUTS, glue("sel_{regime}_{sulfide}_{carbonate}_{DOC_label}_{transition}.txt"))
  
  pqi_text <- glue(
    "SOLUTION 1
    pH {pH}
    pe 4
    temp 25
    redox pe
    units mol/kgw
    Fe(3) 1e-5
    Fe(2) 1e-6
    C(4) {ifelse(carbonate == 'withCarbonate', 1e-3, 0)}
    S(-2) {ifelse(sulfide == 'withSulfide', 1e-3, 0)}
    Acetate {DOC_value}

EQUILIBRIUM_PHASES
    Ferrihydrite 0 0
    Goethite     0 0
    Pyrite       0 0
    Siderite     0 0

SELECTED_OUTPUT
    -file \"{selout}\"
    -reset false
    -high_precision true
    -user_punch true

USER_PUNCH
    -headings regime pH_set sulfide carbonate DOC_label transition pH pe Fe_total Fe2 Fe3 SI_Ferrihydrite SI_Goethite SI_Pyrite SI_Siderite
10  PUNCH \"{regime}\"
20  PUNCH {pH}
30  PUNCH \"{sulfide}\"
40  PUNCH \"{carbonate}\"
50  PUNCH \"{DOC_label}\"
60  PUNCH \"{transition}\"
70  PUNCH -LA(\"H+\")
80  PUNCH -LA(\"e-\")
90  PUNCH TOT(\"Fe\")
100 PUNCH TOT(\"Fe(2)\")
110 PUNCH TOT(\"Fe(3)\")
120 PUNCH SI(\"Ferrihydrite\")
130 PUNCH SI(\"Goethite\")
140 PUNCH SI(\"Pyrite\")
150 PUNCH SI(\"Siderite\")
END
")
  writeLines(pqi_text, fpath, useBytes = TRUE)
  invisible(fpath)
}

cat("🧩 Writing PHREEQC input files (.pqi)...\n")
pwalk(grid, write_pqi)
cat("✅ All .pqi files created.\n\n")

# ---- 4. RUN PHREEQC INLINE (force rerun if missing .txt) -----------------
cat("🚀 Running PHREEQC inline for all grid conditions...\n")

pqi_files <- dir_ls(WORKDIR, glob = "*.pqi")

for (p in pqi_files) {
  base <- path_ext_remove(path_file(p))
  out_path <- file.path(OUTPUTS, glue("{base}.out"))
  sel_txt  <- file.path(OUTPUTS, glue("sel_{str_remove(base, '^fe_')}.txt"))
  
  # rerun only if selected_output missing
  if (file_exists(sel_txt)) {
    cat(glue("⏭️  Skipping (sel exists): {basename(sel_txt)}\n"))
    next
  }
  
  cat(glue("▶ Running: {basename(p)}\n"))
  dir_create(OUTPUTS, recurse = TRUE)
  cmd <- glue("\"{PHREEQC_EXE}\" \"{p}\" \"{out_path}\" \"{DB_PATH}\"")
  system(cmd, wait = TRUE)
}
cat("✅ Inline PHREEQC runs completed.\n\n")

# ---- 5. PARSE SELECTED_OUTPUT FILES --------------------------------------
sel_files <- dir_ls(OUTPUTS, regexp = "^sel_.*\\.txt$")
if (length(sel_files) == 0)
  stop("❌ No SELECTED_OUTPUT .txt files found — check PHREEQC outputs.")

read_sel <- function(f) {
  if (file.size(f) == 0) return(NULL)
  dat <- tryCatch(read_tsv(f, show_col_types = FALSE, trim_ws = TRUE), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  dat <- mutate(dat, across(everything(), ~ str_squish(as.character(.x))))
  dat$file <- basename(f)
  dat
}

cat(glue("📥 Reading {length(sel_files)} SELECTED_OUTPUT files...\n"))
sel_data <- map(sel_files, read_sel) %>% compact() %>% list_rbind()

cat(glue("✅ Parsed {nrow(sel_data)} rows from {length(sel_files)} files.\n\n"))

# ---- 6. CLEANUP & WRITE RESULTS ------------------------------------------
num_cols <- c("pH_set","pH","pe","Fe_total","Fe2","Fe3",
              "SI_Ferrihydrite","SI_Goethite","SI_Pyrite","SI_Siderite")
for (n in num_cols) if (n %in% names(sel_data))
  sel_data[[n]] <- suppressWarnings(as.numeric(sel_data[[n]]))

chr_cols <- c("regime","sulfide","carbonate","DOC_label","transition")
for (cc in intersect(chr_cols, names(sel_data)))
  sel_data[[cc]] <- as.character(sel_data[[cc]])

sel_data <- sel_data %>%
  select(where(~ !(all(is.na(.x))))) %>%
  select(-matches("^\\.\\.\\.[0-9]+$"), everything())

out_tsv <- file.path(WORKDIR, "Fe_redox_model_results_clean.tsv")
write_tsv(sel_data, out_tsv)
cat(glue("💾 Results saved to: {out_tsv}\n"))

# ---- 7. QUICK SUMMARY ----------------------------------------------------
cat("🔎 Summary by condition combination:\n")
sel_data %>%
  count(regime, sulfide, carbonate, DOC_label, transition, name = "rows") %>%
  arrange(regime, sulfide, carbonate, DOC_label, transition) %>%
  print(n = 10)

cat("\n💡 Top 10 by SI_Ferrihydrite:\n")
sel_data %>%
  arrange(desc(SI_Ferrihydrite)) %>%
  select(regime, sulfide, carbonate, DOC_label, transition, pH, SI_Ferrihydrite) %>%
  head(10) %>%
  print()

cat("\n🎉 Workflow completed successfully.\n")

