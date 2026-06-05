# ======================================================================
# Fe(II)(aq) products grid using PHREEQC with phreeqc.dat (stable version)
# - Redox: Oxic vs Anoxic
# - pH bands: 4.6–6, 6–7.5, 7.5–10 (sampled)
# - DOC levels recorded but NOT injected as an organic ligand in phreeqc.dat
# - Ligands toggled via inorganic C (carbonate) and S(-II) (sulfide)
# - Auto-detects valid phase names in DB; uses only those for EQ phases and SI
# - Outputs: CSV + 3×3 SI figure
# ======================================================================

# ---- macOS PHREEQC paths ---------------------------------------------
PHREEQC_EXE <- "/Applications/phreeqc-3.5.0-14000/bin/phreeqc"
if (!file.exists(PHREEQC_EXE)) {
  stop("PHREEQC executable not found at: ", PHREEQC_EXE,
       "\nApprove it in System Settings > Privacy & Security or fix this path.")
}

# Database: stick with phreeqc.dat for maximum stability
DB_PATH <- "/Applications/phreeqc-3.5.0-14000/database/phreeqc.dat"
if (!file.exists(DB_PATH)) stop("Database not found at: ", DB_PATH)

# If you later switch to MINTEQ for organics, set:
# DB_PATH <- "/Applications/phreeqc-3.5.0-14000/database/minteq.v4.dat"
# and re-enable the DOC ligand block noted below.

# ---- Libraries --------------------------------------------------------
suppressPackageStartupMessages({
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr",  quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tidyr",  quietly = TRUE)) install.packages("tidyr")
  if (!requireNamespace("purrr",  quietly = TRUE)) install.packages("purrr")
  if (!requireNamespace("readr",  quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("glue",   quietly = TRUE)) install.packages("glue")
})
library(ggplot2); library(dplyr); library(tidyr); library(purrr); library(readr); library(glue)

# ---- Working directory ------------------------------------------------
WORKDIR <- "phreeqc_fe_products"
dir.create(WORKDIR, showWarnings = FALSE)

# ---- Experimental design ---------------------------------------------
grid_pH <- list(
  `4.6–6`   = c(4.6, 5.2, 5.8),
  `6–7.5`   = c(6.0, 6.8, 7.4),
  `7.5–10`  = c(7.6, 8.5, 9.6)
)

DOC_defs <- tibble::tibble(
  DOC_label = c("Low DOC (<2 mg C/L)", "Moderate DOC (2–8 mg C/L)", "High DOC (>8 mg C/L)"),
  DOC_mgC_L = c(1.0, 5.0, 15.0)
)

redox_defs <- tibble::tibble(
  redox = c("Oxic", "Anoxic"),
  Eh_V  = c(0.5, -0.15)
)

ligand_defs <- tibble::tibble(
  ligands       = c("none", "carbonate", "sulfide", "carb+sulf"),
  add_carbonate = c(FALSE,   TRUE,        FALSE,     TRUE),
  add_sulfide   = c(FALSE,   FALSE,       TRUE,      TRUE)
)

# ---- Chemistry knobs --------------------------------------------------
Fe_total_mgL <- 5       # total Fe (mg/L as Fe)
Na_mgL <- 10; Cl_mgL <- 10

CARB_mgC_L <- 20        # add as C(4) when carbonate toggled (mg C/L)
SULF_mgS_L <- 1         # add as S(-2) when sulfide toggled (mg S/L)

# Eh to pe (25°C): pe ≈ Eh/0.05916
Eh_to_pe <- function(Eh) Eh / 0.05916

# ---- Safe phase-name detection (locale-agnostic) ---------------------
pick_phase <- function(db_path, candidates) {
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", old), silent = TRUE), add = TRUE)
  try(Sys.setlocale("LC_CTYPE", "C"), silent = TRUE)
  txt <- readLines(db_path, warn = FALSE, skipNul = TRUE)
  hits <- character(0)
  for (cand in candidates) {
    if (any(substr(txt, 1L, nchar(cand)) == cand)) hits <- c(hits, cand)
  }
  unique(hits)
}

# Detect phases present in your phreeqc.dat
FE_OH3_PHASE   <- pick_phase(DB_PATH, c("Fe(OH)3(a)", "Ferrihydrite", "Fe(OH)3"))
FES_PHASE      <- pick_phase(DB_PATH, c("Mackinawite", "FeS(ppt)", "FeS(s)", "FeS"))
PYRITE_PHASE   <- pick_phase(DB_PATH, c("Pyrite"))
GOETHITE_PHASE <- pick_phase(DB_PATH, c("Goethite"))   # may be empty in your DB
HEMATITE_PHASE <- pick_phase(DB_PATH, c("Hematite"))
SIDERITE_PHASE <- pick_phase(DB_PATH, c("Siderite"))

first_or_null <- function(x) if (length(x)) x[1] else NULL

PHASE_EQ <- c(
  first_or_null(FE_OH3_PHASE),
  first_or_null(GOETHITE_PHASE),
  first_or_null(HEMATITE_PHASE),
  first_or_null(SIDERITE_PHASE),
  first_or_null(FES_PHASE),
  first_or_null(PYRITE_PHASE)
)
PHASE_EQ <- PHASE_EQ[!is.na(PHASE_EQ)]
PHASE_SI <- PHASE_EQ

if (!length(PHASE_EQ)) stop("No target phases detected in ", DB_PATH)

message("Using phases: ", paste(PHASE_EQ, collapse = ", "))

# ---- PHREEQC block builder (no organic ligand in phreeqc.dat) --------
make_block <- function(id, pH, Eh, DOC_mgC_L, add_carbonate, add_sulfide) {
  id_i <- as.integer(round(id))
  pe   <- Eh_to_pe(Eh)
  
  carb_line <- if (add_carbonate) sprintf("    C(4)  %g as C", CARB_mgC_L) else ""
  sulf_line <- if (add_sulfide)   sprintf("    S(-2) %g as S", SULF_mgS_L) else ""
  
  # NOTE: With phreeqc.dat we do NOT inject a DOC ligand. We still record DOC_mgC_L in USER_PUNCH.
  # If you later switch to minteq.v4.dat and want a ligand, add a REACTION block there.
  
  eq_lines <- paste(sprintf("    %s   0 0", PHASE_EQ), collapse = "\n")
  si_line  <- paste(PHASE_SI, collapse = " ")
  
  lig_str <- if (add_carbonate && add_sulfide) "carb+sulf" else
    if (add_carbonate) "carbonate" else
      if (add_sulfide) "sulfide" else "none"
  
  sprintf('
SOLUTION %i
    temp 25
    pH %g charge
    pe %g
    units mg/L
    Na %g
    Cl %g
    Fe %g as Fe
%s
%s

EQUILIBRIUM_PHASES %i
%s

SELECTED_OUTPUT %i
    -reset               false
    -high_precision      true
    -pH                  true
    -pe                  true
    -ionic_strength      true
    -totals              Fe C S
    -si                  %s

USER_PUNCH %i
    -headings ID pH Eh DOC_mgC_L ligands carbonate sulfide
10 PUNCH %i, %g, %g, %g, "%s", %i, %i
END
', id_i, pH, pe, Na_mgL, Cl_mgL, Fe_total_mgL,
          carb_line, sulf_line,
          id_i, eq_lines,
          id_i, si_line,
          id_i,
          id_i, pH, Eh, DOC_mgC_L, lig_str, as.integer(add_carbonate), as.integer(add_sulfide))
}

# ---- Build the full grid ----------------------------------------------
grid <- tidyr::expand_grid(
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

# ---- Write PHREEQC input ----------------------------------------------
infile  <- file.path(WORKDIR, "fe_grid.pqi")
outfile <- file.path(WORKDIR, "fe_grid.out")
selfile <- file.path(WORKDIR, "fe_grid.sel")
writeLines("", con = infile)

for (i in seq_len(nrow(grid))) {
  blk <- make_block(
    id = grid$ID[i],
    pH = grid$pH[i],
    Eh = grid$Eh_V[i],
    DOC_mgC_L = grid$DOC_mgC_L[i],
    add_carbonate = grid$add_carbonate[i],
    add_sulfide   = grid$add_sulfide[i]
  )
  cat(blk, file = infile, append = TRUE)
}

# ---- Run PHREEQC ------------------------------------------------------
cmd <- sprintf('"%s" "%s" "%s" "%s"', PHREEQC_EXE, infile, outfile, DB_PATH)
message("Running: ", cmd)
status <- system(cmd)
if (status != 0) stop("PHREEQC run failed. Check PHREEQC_EXE/DB_PATH and macOS approval of the binary.")

# ---- Parse Selected Output --------------------------------------------
if (!file.exists(selfile)) stop("Selected output file not found: ", selfile)

raw <- tryCatch(
  read.table(selfile, header = TRUE, sep = "", check.names = FALSE),
  error = function(e) { stop("Could not read selected output: ", conditionMessage(e)) }
)
if (nrow(raw) == 0) stop("Selected output is empty; verify phase names exist in ", DB_PATH)

names(raw) <- gsub("\\.", "_", names(raw))

# ---- Merge with grid, save results ------------------------------------
out <- grid %>% select(ID, pH_band, DOC_label, redox, ligands) %>%
  left_join(raw, by = c("ID" = "ID"))
readr::write_csv(out, file.path(WORKDIR, "fe_products_results.csv"))
message("Wrote: ", normalizePath(file.path(WORKDIR, "fe_products_results.csv")))

# ---- 3×3 SI figure (rows=pH band; cols=DOC level) ---------------------
si_candidates <- grep("^SI_", names(out), value = TRUE)
if (length(si_candidates) == 0) {
  warning("No SI_ columns found; skipping SI plot.")
} else {
  si_long <- out %>%
    select(pH_band, DOC_label, redox, ligands, pH, all_of(si_candidates)) %>%
    pivot_longer(cols = all_of(si_candidates), names_to = "mineral", values_to = "SI")
  
  p3 <- ggplot(si_long, aes(pH, SI, color = redox, group = interaction(redox, ligands))) +
    geom_line(alpha = 0.85) +
    facet_grid(rows = vars(pH_band), cols = vars(DOC_label)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(x = "pH", y = "Saturation Index (SI)",
         title = "Fe mineral saturation indices across pH bands × DOC (colored by redox)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
  ggsave(file.path(WORKDIR, "Fe_SI_3x3_pHband_DOC.png"), p3, width = 7, height = 4.167, dpi = 600)
}

message("Done.")








# ======================================================================
# run_FeII_products_grid_MAC_SAFE_phreeqc_SELWRITE.R
# Fe(II)(aq) product grid for phreeqc.dat (macOS)
#   • Auto-detects valid phases
#   • Writes both .out and .sel files
#   • No organic ligand injection (phreeqc.dat-safe)
# ======================================================================

# ---- macOS PHREEQC paths ---------------------------------------------
PHREEQC_EXE <- "/Applications/phreeqc-3.5.0-14000/bin/phreeqc"
if (!file.exists(PHREEQC_EXE))
  stop("PHREEQC executable not found at: ", PHREEQC_EXE)

DB_PATH <- "/Applications/phreeqc-3.5.0-14000/database/phreeqc.dat"
if (!file.exists(DB_PATH)) stop("Database not found at: ", DB_PATH)

# ---- Libraries --------------------------------------------------------
suppressPackageStartupMessages({
  pkgs <- c("ggplot2","dplyr","tidyr","purrr","readr","glue")
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
})
library(ggplot2); library(dplyr); library(tidyr); library(purrr); library(readr); library(glue)

# ---- Working directory ------------------------------------------------
WORKDIR <- "phreeqc_fe_products"
# Ensure absolute path and directory creation
WORKDIR <- normalizePath(WORKDIR, mustWork = FALSE)
dir.create(WORKDIR, showWarnings = FALSE, recursive = TRUE)
message("Working directory for PHREEQC files: ", WORKDIR)

# ---- Design grid ------------------------------------------------------
grid_pH <- list(`4.6–6`=c(4.6,5.2,5.8), `6–7.5`=c(6.0,6.8,7.4), `7.5–10`=c(7.6,8.5,9.6))
DOC_defs <- tibble(DOC_label=c("Low DOC (<2 mg C/L)","Moderate DOC (2–8 mg C/L)","High DOC (>8 mg C/L)"),
                   DOC_mgC_L=c(1,5,15))
redox_defs <- tibble(redox=c("Oxic","Anoxic"), Eh_V=c(0.5,-0.15))
ligand_defs <- tibble(ligands=c("none","carbonate","sulfide","carb+sulf"),
                      add_carbonate=c(FALSE,TRUE,FALSE,TRUE),
                      add_sulfide=c(FALSE,FALSE,TRUE,TRUE))

# ---- Constants --------------------------------------------------------
Fe_total_mgL <- 5; Na_mgL <- 10; Cl_mgL <- 10
CARB_mgC_L <- 20; SULF_mgS_L <- 1
Eh_to_pe <- function(Eh) Eh / 0.05916

# ---- Safe phase detection --------------------------------------------
pick_phase <- function(db_path, candidates){
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(try(Sys.setlocale("LC_CTYPE", old), silent=TRUE), add=TRUE)
  try(Sys.setlocale("LC_CTYPE","C"), silent=TRUE)
  txt <- readLines(db_path, warn=FALSE, skipNul=TRUE)
  hits <- character(0)
  for(cand in candidates)
    if(any(substr(txt,1L,nchar(cand))==cand)) hits <- c(hits,cand)
  unique(hits)
}
FE_OH3_PHASE   <- pick_phase(DB_PATH,c("Fe(OH)3(a)","Ferrihydrite","Fe(OH)3"))
FES_PHASE      <- pick_phase(DB_PATH,c("Mackinawite","FeS(ppt)","FeS(s)","FeS"))
PYRITE_PHASE   <- pick_phase(DB_PATH,"Pyrite")
GOETHITE_PHASE <- pick_phase(DB_PATH,"Goethite")
HEMATITE_PHASE <- pick_phase(DB_PATH,"Hematite")
SIDERITE_PHASE <- pick_phase(DB_PATH,"Siderite")
first_or_null <- function(x) if(length(x)) x[1] else NULL
PHASE_EQ <- c(first_or_null(FE_OH3_PHASE),first_or_null(GOETHITE_PHASE),
              first_or_null(HEMATITE_PHASE),first_or_null(SIDERITE_PHASE),
              first_or_null(FES_PHASE),first_or_null(PYRITE_PHASE))
PHASE_EQ <- PHASE_EQ[!is.na(PHASE_EQ)]
PHASE_SI <- PHASE_EQ
if(!length(PHASE_EQ)) stop("No valid phases detected in ",DB_PATH)
message("Using phases: ", paste(PHASE_EQ,collapse=", "))

# ---- File paths -------------------------------------------------------
infile  <- file.path(WORKDIR,"fe_grid.pqi")
outfile <- file.path(WORKDIR,"fe_grid.out")
selfile <- file.path(WORKDIR,"fe_grid.sel")
sel_path <- normalizePath(selfile, mustWork=FALSE)

# ---- Block builder ----------------------------------------------------
make_block <- function(id, pH, Eh, DOC_mgC_L, add_carbonate, add_sulfide) {
  id_i <- as.integer(round(id))
  pe   <- Eh_to_pe(Eh)
  
  carb_line <- if (add_carbonate) sprintf("    C(4)  %g as C", CARB_mgC_L) else ""
  sulf_line <- if (add_sulfide)   sprintf("    S(-2) %g as S", SULF_mgS_L) else ""
  
  eq_lines <- paste(sprintf("    %s   0 0", PHASE_EQ), collapse = "\n")
  si_line  <- paste(PHASE_SI, collapse = " ")
  
  lig_str <- if (add_carbonate && add_sulfide) "carb+sulf" else
    if (add_carbonate) "carbonate" else
      if (add_sulfide) "sulfide" else "none"
  
  # Ensure the selected-output file path is quoted for PHREEQC
  sel_quoted <- sprintf('"%s"', normalizePath(selfile, mustWork = FALSE))
  
  sprintf('
SOLUTION %i
    temp 25
    pH %g charge
    pe %g
    units mg/L
    Na %g
    Cl %g
    Fe %g as Fe
%s
%s

EQUILIBRIUM_PHASES %i
%s

SELECTED_OUTPUT %i %s
    -reset               false
    -high_precision      true
    -pH                  true
    -pe                  true
    -ionic_strength      true
    -totals              Fe C S
    -si                  %s

USER_PUNCH %i
    -headings ID pH Eh DOC_mgC_L ligands carbonate sulfide
10 PUNCH %i, %g, %g, %g, "%s", %i, %i
END
',
          id_i, pH, pe, Na_mgL, Cl_mgL, Fe_total_mgL,
          carb_line, sulf_line,
          id_i, eq_lines,
          id_i, sel_quoted,
          si_line,
          id_i,
          id_i, pH, Eh, DOC_mgC_L, lig_str, as.integer(add_carbonate), as.integer(add_sulfide))
}

# ---- Build grid -------------------------------------------------------
grid <- tidyr::expand_grid(
  pH_band=names(grid_pH),
  pH=unlist(grid_pH,use.names=FALSE),
  DOC_label=DOC_defs$DOC_label,
  DOC_mgC_L=DOC_defs$DOC_mgC_L,
  redox=redox_defs$redox,
  Eh_V=redox_defs$Eh_V,
  ligands=ligand_defs$ligands
) %>%
  left_join(ligand_defs,by="ligands") %>%
  arrange(pH_band,DOC_label,redox,ligands,pH) %>%
  mutate(ID=row_number())

# ---- Write PHREEQC input ----------------------------------------------
writeLines("",con=infile)
for(i in seq_len(nrow(grid))){
  blk <- make_block(grid$ID[i],grid$pH[i],grid$Eh_V[i],grid$DOC_mgC_L[i],
                    grid$add_carbonate[i],grid$add_sulfide[i])
  cat(blk,file=infile,append=TRUE)
}

# ---- Run PHREEQC ------------------------------------------------------
cmd <- sprintf('"%s" "%s" "%s" "%s"',PHREEQC_EXE,infile,outfile,DB_PATH)
message("Running: ",cmd)
status <- system(cmd)
if(status!=0) stop("PHREEQC run failed. Check PHREEQC_EXE/DB_PATH approval.")

# ---- Parse Selected Output --------------------------------------------
if(!file.exists(selfile)) stop("Selected output file not found: ", selfile)
raw <- read.table(selfile, header=TRUE, sep="", check.names=FALSE)
names(raw) <- gsub("\\.","_",names(raw))
out <- grid %>% select(ID,pH_band,DOC_label,redox,ligands) %>% left_join(raw,by="ID")
readr::write_csv(out,file.path(WORKDIR,"fe_products_results.csv"))
message("Wrote: ", normalizePath(file.path(WORKDIR,"fe_products_results.csv")))

# ---- 3×3 SI plot ------------------------------------------------------
si_candidates <- grep("^SI_",names(out),value=TRUE)
if(length(si_candidates)>0){
  si_long <- out %>%
    select(pH_band,DOC_label,redox,ligands,pH,all_of(si_candidates)) %>%
    pivot_longer(cols=all_of(si_candidates),names_to="mineral",values_to="SI")
  p3 <- ggplot(si_long,aes(pH,SI,color=redox,group=interaction(redox,ligands)))+
    geom_line(alpha=0.85)+
    facet_grid(rows=vars(pH_band),cols=vars(DOC_label))+
    geom_hline(yintercept=0,linetype="dashed",color="gray50")+
    labs(x="pH",y="Saturation Index (SI)",
         title="Fe mineral SIs across pH bands × DOC (colored by redox)")+
    theme_minimal(base_size=10)+theme(legend.position="bottom")
  ggsave(file.path(WORKDIR,"Fe_SI_3x3_pHband_DOC.png"),p3,width=7,height=4.167,dpi=600)
}
message("Done.")

list.files("phreeqc_fe_products", pattern = "fe_grid", full.names = TRUE)
