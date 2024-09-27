#!/bin/bash

# Define the hardcoded paths for the files
ZSHRC_PATH="$HOME/.config/zsh/.zshrc"
ZELLIJ_CONFIG_PATH="$HOME/.config/zellij/config.kdl"
SHIVAS_NOTE="$HOME/.config/zsh/scripts/commands.md"

# Function to show the file content
show_file() {
  local file_path=$1
  if [ -f "$file_path" ]; then
    # open the file in vim, in read only mode,
    # just so that this command is used to only view
    nvim -R "$file_path"
    # cat "$file_path"
  else
    echo "File not found: $file_path"
  fi
}

# Function to display the help message
show_help() {
  echo "Usage: show {zsh|zellij|-h|--help}"
  echo "Options:"
  echo "  zsh           Show the .zshrc file."
  echo "  zellij        Show the Zellij config file."
  echo "  note | notes  Show the Zellij config file."
  echo "  -h, --help    Show this help message."
}

# Check the argument passed to the script
case "$1" in
zsh)
  show_file "$ZSHRC_PATH"
  ;;
zellij)
  show_file "$ZELLIJ_CONFIG_PATH"
  ;;
-h | --help)
  show_help
  ;;
note | notes)
  show_file "$SHIVAS_NOTE"
  ;;
*)
  show_help
  ;;
esac
