#!/usr/bin/env fish

function watchcpp
    # Check if we have at least 2 arguments
    if test (count $argv) -lt 2
        echo "Usage: watchcpp <file/directory> <command...>"
        echo "Example: watchcpp ./src 'g++ main.cpp -o test && ./test'"
        return 1
    end

    # First argument is the file/directory to watch
    set watch_path $argv[1]

    # Rest of the arguments form the command to execute
    set -e argv[1]
    set command $argv

    # Check if watch path exists
    if not test -e $watch_path
        echo "Error: $watch_path does not exist"
        return 1
    end

    # Initial command run
    echo "Initial run of command..."
    eval $command

    echo "Watching $watch_path for changes..."
    echo "Will execute: $command"

    while true
        # Use fswatch to monitor file changes
        # -1 means exit after first event
        # -r means recursive (if directory)
        fswatch -1 -r $watch_path >/dev/null

        # Clear screen for better visibility
        clear

        # Print timestamp
        date "+%Y-%m-%d %H:%M:%S"
        echo "Change detected in $watch_path"
        echo "Running command..."

        # Run the command
        eval $command

        echo \n"Watching for more changes..."
    end
end
