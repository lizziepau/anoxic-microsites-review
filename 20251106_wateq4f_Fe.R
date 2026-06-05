# ============================================================
# PHREEQC Fe Redox Shift Model — Verified for macOS
# ============================================================
suppressPackageStartupMessages({
  library(glue)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

# ---- PATHS ---------------------------------------------------------------
PHREEQC_EXE <- "/usr/local/bin/phreeqc"
DB_PATH     <- "/usr/local/share/doc/IPhreeqc/database/phreeqc.dat"
BASE_DIR    <- "/Users/epaulus/Documents/PHREEQC/ConceptModel"
timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
WORKDIR     <- file.path(BASE_DIR, glue("Fe_redox_{timestamp}"))
dir.create(WORKDIR, showWarnings = FALSE, recursive = TRUE)
setwd(WORKDIR)

# ---- FUNCTION ------------------------------------------------------------
run_phreeqc <- function(tag, o2_start, o2_end, sel_filename) {
  infile   <- glue("fe_{tag}.pqi")
  outfile  <- glue("fe_{tag}.out")
  sel_file <- file.path(WORKDIR, sel_filename)
  
  cat(glue(
    'SELECTED_OUTPUT
  -file {sel_file}
  -reset false
  -high_precision true
  -pH true
  -pe true
  -si Goethite Hematite Siderite Pyrite Vivianite
  -totals Fe Fe(2) Fe(3) O(0)
  -molalities Fe+2 Fe+3 FeOH+ Fe(OH)2 Fe(OH)3- Fe(OH)4-

# --- Step 1: initial equilibrium (oxic or anoxic) ---
SOLUTION 1
  temp 25
  pH 7
  pe 10
  units mmol/kgw
  Na 1
  Cl 1 charge
  Fe(3) 1e-5
  Fe(2) 1e-6
  Ca 1e-4
EQUILIBRIUM_PHASES 1
  O2(g) {o2_start} 0
END

# --- Step 2: redox shift (re-equilibrate) ---
USE solution 1
EQUILIBRIUM_PHASES 2
  O2(g) {o2_end} 0
END
'), file = infile, append = FALSE)
  
  message(glue("▶ Running {tag} ..."))
  status <- system(glue('"{PHREEQC_EXE}" "{infile}" "{outfile}" "{DB_PATH}"'))
  
  if (status == 0 && file.exists(sel_file) && file.info(sel_file)$size > 0) {
    message(glue("✅ {tag} completed; reading output."))
    dat <- read_table(sel_file, comment = "#", col_names = TRUE, show_col_types = FALSE) |>
      mutate(
        Cycle = tag,
        Step  = row_number(),
        Fe2_to_Fe3 = `-totals_Fe(2)` / pmax(`-totals_Fe(3)`, 1e-30)
      ) |>
      rename(pH = `-pH`, pe = `-pe`) |>
      select(Cycle, Step, pH, pe, Fe2_to_Fe3,
             starts_with("-totals_"), starts_with("-molalities_"), everything())
    print(dat, n = 10)
    return(dat)
  } else {
    warning(glue("⚠️ {tag}: no SELECTED_OUTPUT found. Check {outfile}"))
    if (file.exists(outfile)) {
      cat("\n---- First 40 lines of PHREEQC .out ----\n")
      cat(paste(readLines(outfile, n = 40), collapse = "\n"))
      cat("\n----------------------------------------\n")
    }
    return(NULL)
  }
}

# ---- RUN BOTH DIRECTIONS -------------------------------------------------
oxic_to_anoxic <- run_phreeqc(
  tag = "oxic_to_anoxic",
  o2_start = -1,     # oxic
  o2_end   = -36,    # anoxic
  sel_filename = "sel_oxic_to_anoxic.txt"
)

anoxic_to_oxic <- run_phreeqc(
  tag = "anoxic_to_oxic",
  o2_start = -36,    # anoxic
  o2_end   = -1,     # oxic
  sel_filename = "sel_anoxic_to_oxic.txt"
)

# ---- COMBINE & EXPORT ----------------------------------------------------
combined_df <- bind_rows(oxic_to_anoxic, anoxic_to_oxic)
if (!is.null(combined_df) && nrow(combined_df) > 0) {
  write_csv(combined_df, "Fe_redox_combined.csv")
  message("✅ Combined results saved as Fe_redox_combined.csv")
  
  p <- combined_df |>
    ggplot(aes(x = Step, y = Fe2_to_Fe3, color = Cycle, group = Cycle)) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.8) +
    labs(
      x = "Step (1 = initial, 2 = shifted)",
      y = "Fe(II) / Fe(III)",
      title = "Fe Redox Shifts: Oxic ↔ Anoxic (Single-Step)",
      color = NULL
    ) +
    theme_bw(base_size = 14)
  
  ggsave("Fe_redox_cycles.png", p, width = 7, height = 5, dpi = 300)
  message("📊 Plot saved as Fe_redox_cycles.png")
} else {
  warning("⚠️ No valid data to combine.")
}

message(glue("\n📁 Results saved in {WORKDIR}"))























# ============================================================
# Combine MacPhreeqc Fe Redox Shift Results (Oxic ↔ Anoxic)
# (robust parsing for multi-line pe/pH blocks)
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

# ---- PATHS -------------------------------------------------
file_anoxic_to_oxic <- "~/Documents/PHREEQC/ConceptModel/MacPhreeqc Test/output_fe_anoxic_to_oxic.txt"
file_oxic_to_anoxic <- "~/Documents/PHREEQC/ConceptModel/MacPhreeqc Test/output_fe_oxic_to_anoxic.txt"

# ---- PARSING FUNCTION --------------------------------------
extract_redox <- function(file, tag) {
  lines <- readLines(file)
  
  # Keep only lines that contain clear numeric entries
  ph_lines <- lines[grepl("pH\\s*=", lines)]
  pe_lines <- lines[grepl("pe\\s*=", lines)]
  fe2_lines <- lines[grepl("Fe\\(2\\)", lines)]
  fe3_lines <- lines[grepl("Fe\\(3\\)", lines)]
  
  # Extract numeric values from these lines
  extract_num <- function(x) as.numeric(str_extract(x, "-?\\d+\\.?\\d*(e[+-]?\\d+)?"))
  
  pH_vals <- extract_num(ph_lines)
  pe_vals <- extract_num(pe_lines)
  Fe2_vals <- extract_num(fe2_lines)
  Fe3_vals <- extract_num(fe3_lines)
  
  # Only keep the first two equilibrated states if extras appear
  n <- min(length(pH_vals), length(pe_vals), length(Fe2_vals), length(Fe3_vals))
  pH_vals <- head(pH_vals, n)
  pe_vals <- head(pe_vals, n)
  Fe2_vals <- head(Fe2_vals, n)
  Fe3_vals <- head(Fe3_vals, n)
  
  tibble(
    Cycle = tag,
    Step = seq_len(n),
    pH = pH_vals,
    pe = pe_vals,
    Fe2 = Fe2_vals,
    Fe3 = Fe3_vals,
    Fe2_to_Fe3 = Fe2 / pmax(Fe3, 1e-30)
  )
}

# ---- RUN FOR BOTH DIRECTIONS -------------------------------
df_anox_to_ox <- extract_redox(file_anoxic_to_oxic, "Anoxic → Oxic")
df_ox_to_anox <- extract_redox(file_oxic_to_anoxic, "Oxic → Anoxic")

# ---- COMBINE AND SAVE --------------------------------------
combined_df <- bind_rows(df_anox_to_ox, df_ox_to_anox)
write_csv(combined_df, "~/Documents/PHREEQC/ConceptModel/MacPhreeqc Test/Fe_redox_combined.csv")

# ---- PLOT ---------------------------------------------------
p <- ggplot(combined_df, aes(x = Step, y = Fe2_to_Fe3, color = Cycle, group = Cycle)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  theme_bw(base_size = 14) +
  labs(
    title = "Fe Redox Shifts: Oxic ↔ Anoxic (MacPhreeqc)",
    x = "Step (1 = Initial, 2 = Shifted)",
    y = "Fe(II) / Fe(III)",
    color = NULL
  )

ggsave("~/Documents/PHREEQC/ConceptModel/MacPhreeqc Test/Fe_redox_cycles.png",
       p, width = 7, height = 5, dpi = 300)

message("✅ Combined results and plot generated successfully.")
