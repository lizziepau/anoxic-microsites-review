#!/bin/bash
# ============================================================
# MacPHREEQC Batch Runner — with outputs subfolder (macOS 2025)
# ============================================================

WORKDIR="/Users/epaulus/Documents/PHREEQC/ConceptModel/Fe_redox_grid"
OUTDIR="${WORKDIR}/outputs"
DB_PATH="/Applications/MacPHREEQC.app/Contents/Resources/database/minteq.v4.dat"
PHREEQC_EXE="/Applications/MacPHREEQC.app/Contents/MacOS/phreeqc"
LOGFILE="${WORKDIR}/batch_run_$(date '+%Y%m%d_%H%M%S').log"

# ---- PREPARATION ----
mkdir -p "$OUTDIR"
echo "🚀 Starting MacPHREEQC batch run..." | tee "$LOGFILE"
echo "📂 Working directory: $WORKDIR" | tee -a "$LOGFILE"
echo "📤 Output directory:  $OUTDIR" | tee -a "$LOGFILE"
echo "📘 Database: $(basename "$DB_PATH")" | tee -a "$LOGFILE"
echo "🕒 Started: $(date)" | tee -a "$LOGFILE"
echo "----------------------------------------" | tee -a "$LOGFILE"

cd "$WORKDIR" || { echo "❌ ERROR: Directory not found: $WORKDIR" | tee -a "$LOGFILE"; exit 1; }
shopt -s nullglob
FILES=("$WORKDIR"/*.pqi)
TOTAL=${#FILES[@]}

if [ $TOTAL -eq 0 ]; then
  echo "⚠️ No .pqi files found in: $WORKDIR" | tee -a "$LOGFILE"
  exit 0
fi

# ---- MAIN LOOP ----
i=1
for f in "${FILES[@]}"; do
  base=$(basename "$f" .pqi)
  outfile="${OUTDIR}/${base}.out"

  echo "▶ [$i/$TOTAL] Running: $base.pqi" | tee -a "$LOGFILE"

  # Skip if output already exists
  if [ -f "$outfile" ]; then
    echo "⏭️  [$i/$TOTAL] Skipping (already exists): ${base}.out" | tee -a "$LOGFILE"
    ((i++))
    continue
  fi

  "$PHREEQC_EXE" "$f" "$outfile" "$DB_PATH" >> "$LOGFILE" 2>&1

  if grep -q "End of simulation" "$outfile"; then
    echo "✅ Completed: ${base}" | tee -a "$LOGFILE"
  else
    echo "❌ ERROR in: ${base} (check log)" | tee -a "$LOGFILE"
  fi
  ((i++))
done

echo "----------------------------------------" | tee -a "$LOGFILE"
echo "🎉 Batch run finished at: $(date)" | tee -a "$LOGFILE"
echo "🧾 Log file saved to: $LOGFILE"
echo "📁 Output files saved in: $OUTDIR"
