#!/usr/bin/env fish
# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 12-16-2024  ######################
# ############################################################
# #################  Description  ############################
# This script monitors a specified C file for changes.
# When changes are detected, it compiles and runs the file.

function runc
    # Check if the script received a file path as an argument
    if test (count $argv) -ne 1
        echo "Usage: runc <path-to-c-file>"
        return 1
    end

    # Path of the single C file
    set C_FILE $argv[1]

    # Check if the file exists
    if not test -f $C_FILE
        echo "Error: File '$C_FILE' does not exist."
        return 1
    end

    # Determine the directory and base name of the C file
    set DIR (dirname $C_FILE)
    set BASE (basename $C_FILE .c)

    # Define the output binary path
    set OUT_FILE "$DIR/out"

    # Remove the existing output file if it exists
    if test -f $OUT_FILE
        echo "Removing existing output file: $OUT_FILE"
        rm $OUT_FILE
    end

    # Monitor the file for changes using find and entr
    echo "Watching '$C_FILE' for changes. Press Ctrl+C to stop."
    find $DIR -name (basename $C_FILE) | entr -c fish -c \
        "gcc $C_FILE -o $OUT_FILE; and $OUT_FILE"
end
