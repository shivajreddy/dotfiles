#!/bin/bash

# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 12-6-2024  ######################
# ############################################################
# #################  Description  ############################
# This script monitors a specified C++ file for changes.
# When changes are detected, it compiles and runs the file.
###################################
# 1. Takes the path of a cpp file
# 2. Checks if the file exists
# 3. Checks if 'out' already exists next to it
#      - if so, delete it
# 4. Use find & entr to watch this cpp file,
#    and whenever changes occur, compile it
#    to 'out' and run 'out'
#
# Usage:
#   ./script.sh path/to/file.cpp [cpp-standard-version]
#
# If the cpp-standard-version is not provided, defaults to C++17.
###################################

# Check if the script received a file path as an argument
if [ $# -lt 1 ]; then
    echo "Usage: $0 <path-to-cpp-file> [cpp-version]"
    exit 1
fi

# First argument: path to cpp file
CPP_FILE="$1"

# Second argument is optional, C++ standard version
if [ $# -ge 2 ]; then
    CPP_VERSION="$2"
else
    CPP_VERSION="17"
fi

# Check if the file exists
if [ ! -f "$CPP_FILE" ]; then
    echo "Error: File '$CPP_FILE' does not exist."
    exit 1
fi

# Determine the directory and base name of the C++ file
DIR=$(dirname "$CPP_FILE")
BASE=$(basename "$CPP_FILE" .cpp)

# Define the output binary path
OUT_FILE="$DIR/out"

# Remove the existing output file if it exists
if [ -f "$OUT_FILE" ]; then
    echo "Removing existing output file: $OUT_FILE"
    rm "$OUT_FILE"
fi

# Monitor the file for changes using find and entr
echo "Watching '$CPP_FILE' for changes. Press Ctrl+C to stop."
find "$DIR" -name "$(basename "$CPP_FILE")" | entr -c sh -c "g++ -std=c++$CPP_VERSION $CPP_FILE -o $OUT_FILE && $OUT_FILE"
