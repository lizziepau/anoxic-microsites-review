# ========================================================================
# MacPHREEQC.R — CrossOver-integrated automated Fe redox model
# ========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(fs)
  library(stringr)
  library(purrr)
})

# ---- 1. PATHS ------------------------------------------------------------
BASE_DIR <- path.expand("~/Documents/PHREEQC/ConceptModel")
WORKDIR   <- file.path(BASE_DIR, "Fe_redox_grid")
OUTPUTS   <- file.path(WORKDIR, "outputs")
DB_PATH   <- "/Applications/MacPHREEQC.app/Contents/Resources/database/minteq.v4.dat"
dir_create(OUTPUTS, recurse = TRUE)
setwd(WORKDIR)

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

# ---- 3. PQI WRITER -------------------------------------------------------
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
  message(glue("✅ Created {fname}"))
}

cat("🧩 Generating .pqi input files...\n")
pwalk(grid, write_pqi)
cat("🎉 All .pqi files written.\n\n")


# ---- 4. CROSSOVER PHREEQC RUNNER ----------------------------------------
run_phreeqc_crossover <- function(input_file, output_file, db_path) {
  input_file  <- normalizePath(input_file, mustWork = TRUE)
  output_file <- normalizePath(output_file, mustWork = FALSE)
  db_path     <- normalizePath(db_path, mustWork = TRUE)
  
  win_in  <- gsub("/", "\\\\", input_file)
  win_out <- gsub("/", "\\\\", output_file)
  win_db  <- gsub("/", "\\\\", db_path)
  
  phreeqc_exe <- "C:\\\\Program Files\\\\USGS\\\\phreeqc-3.8.6-17100-x64\\\\bin\\\\ClrRelease\\\\phreeqc.exe"
  
  cmd <- glue::glue(
    '"/Applications/CrossOver.app/Contents/MacOS/CrossOver" --bottle "phreeqc" --run-command ',
    '\'wine "{phreeqc_exe}" "{win_in}" "{win_out}" "{win_db}"\''
  )
  
  message(glue("▶ Running via CrossOver: {basename(input_file)}"))
  system(cmd, wait = TRUE)
}


# ---- 5. RUN ALL PHREEQC MODELS ------------------------------------------
pqi_files <- dir_ls(WORKDIR, glob = "*.pqi")

for (p in pqi_files) {
  dir_create(OUTPUTS, recurse = TRUE)
  out_name <- path_ext_set(path_file(p), "out")
  out_path <- file.path(OUTPUTS, out_name)
  
  if (file_exists(out_path)) {
    cat(glue("⏭️  Skipping (exists): {out_name}\n"))
    next
  }
  
  run_phreeqc_crossover(p, out_path, DB_PATH)
}

cat("✅ All PHREEQC runs completed via CrossOver.\n\n")


# ---- 6. PARSE & CLEAN RESULTS --------------------------------------------
sel_files <- dir_ls(OUTPUTS, regexp = "^sel_.*\\.txt$")
if (length(sel_files) == 0) stop("❌ No SELECTED_OUTPUT .txt files found — verify CrossOver runs completed.")

read_sel <- function(f) {
  if (file.size(f) == 0) return(NULL)
  dat <- tryCatch(read_tsv(f, show_col_types = FALSE, trim_ws = TRUE), error = function(e) NULL)
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  dat$file <- basename(f)
  dat
}

sel_data <- map(sel_files, read_sel) %>% compact() %>% list_rbind()
if (nrow(sel_data) == 0) stop("❌ Parsed no data — check PHREEQC SELECTED_OUTPUT formatting.")

num_cols <- c("pH_set","pH","pe","Fe_total","Fe2","Fe3",
              "SI_Ferrihydrite","SI_Goethite","SI_Pyrite","SI_Siderite")
for (n in num_cols) if (n %in% names(sel_data))
  sel_data[[n]] <- suppressWarnings(as.numeric(sel_data[[n]]))

sel_data <- sel_data %>%
  select(where(~ !(all(is.na(.x))))) %>%
  select(-matches("^\\.\\.\\.[0-9]+$"), everything())

out_tsv <- file.path(WORKDIR, "Fe_redox_model_results_clean.tsv")
write_tsv(sel_data, out_tsv)
cat(glue("💾 Results written to: {out_tsv}\n"))
cat(glue("💡 Total rows parsed: {nrow(sel_data)}\n\n"))
cat("🎉 Workflow completed successfully.\n")
