#!/usr/bin/env fish
# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 12-16-2024  #####################
# ############################################################
# #################  Description  ############################
# This script monitors a specified C file for changes.
# When changes are detected, it compiles and runs the file.

function runc
    # Check if the script received a file path as an argument
    if test (count $argv) -ne 1
        echo "Usage  : runc <path-to-c-file>"
        echo "Example: to run a file in current directory `runc main.c`"
        echo "Usage  : runc <path-to-directory>"
        echo "Example: to run in current directory `runc .`"
        return 1
    end

    # Path of the single C file
    set C_FILE $argv[1]

    # Determine the directory and base name of the C file
    set DIR (dirname $C_FILE)
    set BASE (basename $C_FILE .c)

    # Define the output binary path
    set OUT_FILE "$DIR/out"

    # If the specified C file does not exist,
    # compile and watch all .c files in the directory.
    if not test -f $C_FILE
        echo "File '$C_FILE' not found. Falling back to watch and compile all .c files in $DIR."

        # Remove existing output if present
        if test -f $OUT_FILE
            echo "Removing existing output file: $OUT_FILE"
            rm $OUT_FILE
        end

        echo "Watching '$DIR/*.c' for changes. Press Ctrl+C to stop."
        find $DIR -name '*.c' | entr -c fish -c \
            "gcc $DIR/*.c -o $OUT_FILE; and $OUT_FILE"
        return 0
    end

    # Remove existing output if present
    if test -f $OUT_FILE
        echo "Removing existing output file: $OUT_FILE"
        rm $OUT_FILE
    end

    # Monitor the specified file for changes
    echo "Watching '$C_FILE' for changes. Press Ctrl+C to stop."
    find $DIR -name (basename $C_FILE) | entr -c fish -c \
        "gcc $C_FILE -o $OUT_FILE; and $OUT_FILE"
end
