#!/usr/bin/env bash
# ############################################################
# # Author: Shiva                                           #
# # Date:   12-16-2024                                       #
# ############################################################
# # Description:
# # This script monitors C files for changes using entr.
# # When changes are detected, it compiles and runs.
# #
# # Usage:
# #   ./monitor_c.sh [C-file-to-watch]
# #
# #   - No arguments: looks for main.c in the current dir
# #   - With argument: watch that file’s directory
# ############################################################

compile_all_c_files() {
  local dir="$1"
  local main_file="$2"
  local out_file="$3"

  # Get all .c files except main_file
  local other_files
  other_files=$(find "$dir" -maxdepth 1 -name '*.c' ! -name "$(basename "$main_file")")

  # Compile with additional .c files, if any
  if [ -n "$other_files" ]; then
    echo "Compiling $main_file with additional files: $other_files"
    gcc "$main_file" $other_files -o "$out_file"
  else
    echo "Compiling only $main_file"
    gcc "$main_file" -o "$out_file"
  fi
}

# -----------------------------
# MAIN SCRIPT LOGIC
# -----------------------------

# Detect if we are in "recompile-run" mode
if [ "$1" = "recompile-run" ]; then
  # We already set DIR, MAIN_FILE, OUT_FILE as exports. Perform compile+run:
  compile_all_c_files "$DIR" "$MAIN_FILE" "$OUT_FILE" &&
    exec "$OUT_FILE"
  exit 0
fi

# Otherwise, this is the normal “monitor” mode:

# 1. Figure out which C file we’re monitoring
if [ $# -eq 0 ]; then
  # No argument, default to ./main.c
  DIR="."
  MAIN_FILE="./main.c"
  if [ ! -f "$MAIN_FILE" ]; then
    echo "Error: no main.c found in current directory."
    exit 1
  fi
else
  MAIN_FILE="$1"
  DIR=$(dirname "$MAIN_FILE")
  if [ ! -f "$MAIN_FILE" ]; then
    echo "Error: $MAIN_FILE does not exist."
    exit 1
  fi
fi

OUT_FILE="$DIR/out"

# Remove existing output file if it exists
[ -f "$OUT_FILE" ] && rm "$OUT_FILE"

echo "Watching '$DIR' for changes in .c files. Press Ctrl+C to stop."

# Export some variables so that the child call can see them
export DIR MAIN_FILE OUT_FILE

# 2. Let entr call THIS script again, but with 'recompile-run' argument
find "$DIR" -maxdepth 1 -name '*.c' |
  entr -c ./monitor_c.sh recompile-run
