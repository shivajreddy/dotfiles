#!/bin/bash

echo "🌱 Making sym links to dotfiles"

# Function to create a symbolic link and provide confirmation
create_symlink() {
  local source_path="$1"
  local target_path="$2"

  # Check if source exists
  if [ -e "$source_path" ]; then
    echo "🌱 Creating symbolic link: $target_path -> $source_path"
    ln -sf "$source_path" "$target_path"

    if [ $? -eq 0 ]; then
      echo "✅ Symbolic link created successfully: $target_path -> $source_path"
    else
      echo "❌ Failed to create symbolic link for $target_path. Please check if the paths are correct."
    fi
  else
    echo "❌ Source path $source_path does not exist. Skipping."
  fi
}

# Define source and target paths (update these to use absolute paths)
declare -A links=(
  ["$HOME/dotfiles/wsl/nvim"]="$HOME/.config/nvim"
  ["$HOME/dotfiles/wsl/zellij"]="$HOME/.config/zellij"
  ["$HOME/dotfiles/wsl/htop"]="$HOME/.config/htop"
  ["$HOME/dotfiles/wsl/starship.toml"]="$HOME/.config/starship.toml"
)

# Iterate over each entry in the links array and create symlinks
for source_path in "${!links[@]}"; do
  create_symlink "$source_path" "${links[$source_path]}"
done

echo "🎉 Symbolic links created"

