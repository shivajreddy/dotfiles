#!/usr/bin/env fish
# Define the hardcoded paths for the files
set FISH_CONFIG_PATH "$HOME/.config/fish/config.fish"
set FISH_FUNCTIONS_PATH "$HOME/.config/fish/functions"
set ZELLIJ_CONFIG_PATH "$HOME/.config/zellij/config.kdl"
set SHIVAS_NOTE "$HOME/.config/fish/scripts/commands.md"

# Function to show the file content
function show_file
    set file_path $argv[1]
    if test -f $file_path
        # Open the file in nvim, in read-only mode
        vi -R $file_path
    else
        echo "File not found: $file_path"
    end
end

# Function to show directory content
function show_dir
    set dir_path $argv[1]
    if test -d $dir_path
        # List all fish files in the directory
        set files (find $dir_path -name "*.fish")
        if test (count $files) -gt 0
            # Present a numbered list of files
            for i in (seq (count $files))
                echo "$i: "(basename $files[$i])
            end

            # Ask user to choose a file
            read -P "Enter the number of the file to view (or 'q' to quit): " choice

            if test "$choice" != q
                if test "$choice" -ge 1 -a "$choice" -le (count $files)
                    nvim -R $files[$choice]
                else
                    echo "Invalid selection"
                end
            end
        else
            echo "No fish files found in directory"
        end
    else
        echo "Directory not found: $dir_path"
    end
end

# Function to display the help message
function show_help
    echo "Usage: show {fish|funcs|zellij|note|notes|-h|--help}"
    echo "Options:"
    echo "  fish          Show the Fish config file"
    echo "  funcs         Show Fish function files"
    echo "  zellij        Show the Zellij config file"
    echo "  note | notes  Show the notes file"
    echo "  -h, --help    Show this help message"
end

# Main function
function show
    switch $argv[1]
        case fish
            show_file $FISH_CONFIG_PATH
        case funcs functions
            show_dir $FISH_FUNCTIONS_PATH
        case zellij
            show_file $ZELLIJ_CONFIG_PATH
        case note notes
            show_file $SHIVAS_NOTE
        case -h --help
            show_help
        case '*'
            show_help
    end
end
