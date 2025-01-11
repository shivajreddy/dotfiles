#!/usr/bin/env fish
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
#   runcpp path/to/file.cpp [cpp-standard-version]
#
# If the cpp-standard-version is not provided, defaults to C++17.
###################################

function runcpp
    # Check if the script received a file path as an argument
    if test (count $argv) -lt 1
        echo "Usage: runcpp <path-to-cpp-file> [cpp-version]"
        return 1
    end

    # First argument: path to cpp file
    set CPP_FILE $argv[1]

    # Second argument is optional, C++ standard version
    if test (count $argv) -ge 2
        set CPP_VERSION $argv[2]
    else
        set CPP_VERSION 17
    end

    # Check if the file exists
    if not test -f $CPP_FILE
        echo "Error: File '$CPP_FILE' does not exist."
        return 1
    end

    # Determine the directory and base name of the C++ file
    set DIR (dirname $CPP_FILE)
    set BASE (basename $CPP_FILE .cpp)

    # Define the output binary path
    set OUT_FILE "$DIR/out"

    # Remove the existing output file if it exists
    if test -f $OUT_FILE
        echo "Removing existing output file: $OUT_FILE"
        rm $OUT_FILE
    end

    # Monitor the file for changes using find and entr
    echo "Watching '$CPP_FILE' for changes. Press Ctrl+C to stop."
    find $DIR -name (basename $CPP_FILE) | entr -c fish -c \
        "g++ -std=c++$CPP_VERSION $CPP_FILE -o $OUT_FILE; and $OUT_FILE"
end
