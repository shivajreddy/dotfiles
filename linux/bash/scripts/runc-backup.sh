#!/bin/bash

# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 12-16-2024  ######################
# ############################################################

# #################  Description  ############################
# This script monitors a specified C file for changes.
# When changes are detected, it compiles and runs the file.

###################################
# 1. Takes the path of a c file
# 2. Checks if the file exists
# 3. Checks if 'out' already exists next to it
#      - if so, delete it
# 4. Use find & entr to watch this c file,
#    and whenever there is changes to it, compile it
#    to 'out' and run 'out'
# Example: find ./ -wholename './2024/cpp/day-05/part-02/main.cpp' | entr -c sh -c 'g++ -std=c++17 $(find ./ -wholename "./2024/cpp/day-05/part-02/main.cpp") -o ./2024/cpp/day-05/part-02/out && ./2024/cpp/day-05/part-02/out'
###################################

# Check if the script received a file path as an argument
if [ $# -ne 1 ]; then
  echo "Usage: $0 <path-to-c-file>"
  exit 1
fi

# Path of the single C file
C_FILE="$1"

# Check if the file exists
if [ ! -f "$C_FILE" ]; then
  echo "Error: File '$C_FILE' does not exist."
  exit 1
fi

# Determine the directory and base name of the C++ file
DIR=$(dirname "$C_FILE")
BASE=$(basename "$C_FILE" .c)

# Define the output binary path
OUT_FILE="$DIR/out"

# Remove the existing output file if it exists
if [ -f "$OUT_FILE" ]; then
  echo "Removing existing output file: $OUT_FILE"
  rm "$OUT_FILE"
fi

# Monitor the file for changes using find and entr
echo "Watching '$C_FILE' for changes. Press Ctrl+C to stop."
find "$DIR" -wholename "$C_FILE" | entr -c sh -c \
  "g++ -std=c++17 $C_FILE -o $OUT_FILE && $OUT_FILE"
