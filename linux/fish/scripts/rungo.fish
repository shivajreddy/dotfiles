#!/usr/bin/env fish
# ############################################################
# ###################  Author: Shiva    ######################
# ###################  Date: 03-19-2025  #####################
# ############################################################
# #################  Description  ############################
# This script monitors Go files for changes.
# When changes are detected, it builds and runs the Go program.

function rungo
    # Check if the script received a file path or directory
    if test (count $argv) -ne 1
        echo "Usage  : rungo <path-to-go-file>"
        echo "Example: to run a file in current directory `rungo main.go`"
        echo "Usage  : rungo <path-to-directory>"
        echo "Example: to run in current directory `rungo .`"
        return 1
    end

    # Path of the Go file or directory
    set GO_FILE $argv[1]

    # Determine the directory and base name of the Go file
    set DIR (dirname $GO_FILE)
    set BASE (basename $GO_FILE .go)

    # Monitor and build the specified Go file or directory
    if test -f $GO_FILE
        echo "Watching '$GO_FILE' for changes. Press Ctrl+C to stop."
        find $DIR -name (basename $GO_FILE) | entr -c fish -c \
            "go run $GO_FILE"
    else
        echo "Watching '$DIR/*.go' for changes. Press Ctrl+C to stop."
        find $DIR -name '*.go' | entr -c fish -c \
            "go run $DIR/*.go"
    end
end
