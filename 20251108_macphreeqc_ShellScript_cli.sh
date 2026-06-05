#!/bin/bash
# Wrapper to run MacPHREEQC without prompts
# Usage: macphreeqc_cli.sh input.pqi output.out database.dat

APP="/Applications/MacPHREEQC.app/Contents/MacOS/phreeqc"
"$APP" "$1" "$2" "$3" < /dev/null

