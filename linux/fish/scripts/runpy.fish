#!/usr/bin/env fish
# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 13-1-2025  ######################
# ############################################################
# #################  Description  ############################
# This script monitors a specified Python file for changes.
# When changes are detected, it runs the file automatically.
###################################
# 1. Takes the path of a python file
# 2. Checks if the file exists
# 3. Use find & entr to watch this python file,
#    and whenever changes occur, run it
#
# Usage:
#   runpy path/to/file.py [python-version]
#
# If the python-version is not provided, defaults to python3.
###################################
function runpy
    # Check if the script received a file path as an argument
    if test (count $argv) -lt 1
        echo "Usage: runpy <path-to-python-file> [python-version]"
        return 1
    end

    # First argument: path to python file
    set PY_FILE $argv[1]

    # Second argument is optional, Python version command
    if test (count $argv) -ge 2
        set PY_VERSION $argv[2]
    else
        set PY_VERSION python3
    end

    # Check if the file exists
    if not test -f $PY_FILE
        echo "Error: File '$PY_FILE' does not exist."
        return 1
    end

    # Determine the directory of the Python file
    set DIR (dirname $PY_FILE)

    # Monitor the file for changes using find and entr
    echo "Watching '$PY_FILE' for changes. Press Ctrl+C to stop."
    find $DIR -name (basename $PY_FILE) | entr -c fish -c \
        "$PY_VERSION $PY_FILE"
end
