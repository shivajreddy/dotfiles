#!/usr/bin/env fish
# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 12-6-2024  ######################
# ############################################################
# #################  Description  ############################
# This script monitors a specified C# file for changes.
# When changes are detected, it compiles and runs the file.
###################################
# 1. Takes the path of a C# file
# 2. Checks if the file exists
# 3. Checks if 'out.exe' already exists next to it
#      - if so, delete it
# 4. Use find & entr to watch this C# file,
#    and whenever changes occur, compile it
#    to 'out.exe' and run 'out.exe'
#
# Usage:
#   runcs path/to/file.cs
###################################

function runcs
    # Check if the script received a file path as an argument
    if test (count $argv) -lt 1
        echo "Usage: runcs <path-to-cs-file>"
        return 1
    end

    # First argument: path to C# file
    set CS_FILE $argv[1]

    # Check if the file exists
    if not test -f $CS_FILE
        echo "Error: File '$CS_FILE' does not exist."
        return 1
    end

    # Determine the directory and base name of the C# file
    set DIR (dirname $CS_FILE)
    set BASE (basename $CS_FILE .cs)

    # Define the output binary path
    set OUT_FILE "$DIR/out.exe"

    # Remove the existing output file if it exists
    if test -f $OUT_FILE
        echo "Removing existing output file: $OUT_FILE"
        rm $OUT_FILE
    end

    # Monitor the file for changes using find and entr
    echo "Watching '$CS_FILE' for changes. Press Ctrl+C to stop."
    find $DIR -name (basename $CS_FILE) | entr -c fish -c \
        "csc $CS_FILE -out:$OUT_FILE; and mono $OUT_FILE"
end
