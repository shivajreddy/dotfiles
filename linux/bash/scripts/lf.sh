#!/bin/bash

# commands to work with 'lf'
# make sure 'lf' is already installed

lfcd() {
    local dir
    # Launch lf in choose-dir mode and capture the selected directory
    dir=$(lf --choose-dir="$PWD" 2>/dev/null)

    # Check if a directory was selected
    if [ -d "$dir" ]; then
        cd "$dir"
    fi
}
