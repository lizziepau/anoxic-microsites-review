library(glue)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

PHREEQC_EXE <- "/usr/local/bin/phreeqc"

# Use the phreeqc.dat you’ve been using
DB_PATH <- "/Users/epaulus/Documents/LOAMS_Review/phreeqc/iphreeqc-3.8.6-17100/database/phreeqc.dat"

run_phreeqc <- function(input_core,
                        phreeqc_exe = PHREEQC_EXE,
                        db_path     = DB_PATH,
                        workdir     = tempdir()) {
  
  infile  <- file.path(workdir, paste0("phreeqc_in_",  Sys.getpid(), ".pqi"))
  outfile <- file.path(workdir, paste0("phreeqc_out_", Sys.getpid(), ".txt"))
  selfile <- file.path(workdir, paste0("phreeqc_sel_", Sys.getpid(), ".txt"))
  
  # NOTE: USER_PUNCH + SELECTED_OUTPUT come FIRST, then the SOLUTION
  input_full <- glue("
USER_PUNCH
  -headings pH Fe_tot Fe2 Fe3
  10 PUNCH -LA(\"H+\")              # pH
  20 PUNCH TOT(\"Fe\"), MOL(\"Fe+2\"), MOL(\"Fe+3\")
END

SELECTED_OUTPUT
  -file {selfile}
  -reset false
  -user_punch true
END

{input_core}
")
  
  writeLines(input_full, infile)
  
  # Run PHREEQC
  system2(phreeqc_exe, args = c(infile, outfile, db_path))
  
  # Read the punched table (should now have data rows)
  sel <- readr::read_table(selfile, comment = "#", show_col_types = FALSE)
  sel
}

run_sim <- function(pH, pe = 4, Fe_tot = 1e-3) {
  
  input_core <- glue("
SOLUTION 1
  temp 25
  pH {pH}
  pe {pe}
  units mol/kgw
  Na 0.1
  Cl 0.1
  Fe {Fe_tot}
END
")
  
  out <- run_phreeqc(input_core)
  out %>% mutate(pH_input = pH)
}

test <- run_sim(7)
test

pH_seq <- seq(4, 7, by = 0.25)
grid <- purrr::map_dfr(pH_seq, run_sim)
grid

grid_long <- grid %>%
  transmute(
    pH = as.numeric(pH_input),
    Fe2 = as.numeric(Fe2),
    Fe3 = as.numeric(Fe3)
  ) %>%
  pivot_longer(
    cols      = c(Fe2, Fe3),
    names_to  = "species",
    values_to = "molality"
  )

ggplot(grid_long, aes(x = pH, y = molality, color = species)) +
  geom_line(linewidth = 1.2) +
  scale_y_log10() +
  theme_bw(base_size = 14) +
  labs(
    x = "pH",
    y = "Molality (mol/kgw)",
    title = "Fe(II) and Fe(III) distribution vs pH (phreeqc.dat)"
  )
