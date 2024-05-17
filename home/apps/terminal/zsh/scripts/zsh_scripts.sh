#! /usr/bin/env bash

# use this maybe?
# /usr/bin/env bash
# /usr/bin/sh

# Change to the dotfiles directory
dots() {
    cd $HOME/dotfiles/ || exit
}

# Save changes to the dotfiles repository
savedots() {
    echo " ::::::    Changing to ~/dotfiles    :::::: "
    local current_dir
    current_dir=$(pwd)  # Save the current directory
    cd ~/dotfiles || exit  # Change to dotfiles directory safely

    echo " ::::::         Adding to Git        :::::: "
    # .Use the first positional parameter as the commit message, or use a default message if none is provided
    local commit_message="${1:-Update dotfiles}"
    echo " :::::: Commit Message: ${commit_message} :::::: "

    git add .
    git commit -m "$commit_message"
    git push origin main

    echo " :::::: Changing back to current_dir :::::: "
    cd "$current_dir"  # Return to the original directory
}

# Rebuild NixOS configuration based on the current hostname's flake
osbuild() {
    local hostname
    hostname=$(hostname)  # Capture the hostname in a variable
    echo " ::::::    NixOS-Rebuild for : $hostname    :::::: "
    # echo " ::::::    Path with HostName: $HOME/dotfiles#$hostname :::::: "
    sudo nixos-rebuild switch --flake "$HOME/dotfiles#$hostname"
}

# Home-Manager-Rebuild
rebuild() {
    # Rebuild NixOS configuration based on the current hostname's flake
    local hostname
    hostname=$(hostname)  # Capture the hostname in a variable
    echo " ::::::    Home-Manager-Rebuild : $hostname    :::::: "
    home-manager switch --flake "$HOME/dotfiles"
}

# Todo in pclouddrive
todo() {
  local current_dir
  current_dir=$(pwd)  # Save the current directory

  cd $HOME/obsidianvault/todo/

  nvim todo.md

  # echo " :::::: Changing back to current_dir :::::: "
  cd "$current_dir"  # Return to the original directory

}

# Git Push with optional branch name
gpush () {
  local branch_name="${1:-main}"
  # git push "-u origin ${branch_name}"
  git push
}

# Git Pull with optional branch name
gpull () {
  local branch_name="${1:-main}"
  # git pull "-u origin ${branch_name}"
  git pull
}

# Nix flake update, shorthand for sudo-flake-update
sfu(){
  sudo nix flake update ~/dotfiles
}

garbage(){
  sudo nix-collect-garbage -d
}

news() {
  home-manager news --flake ~/dotfiles
}

myalias() {
  echo "todo -> Open ~/obsidianvault/todo"

  echo""
  echo "gpull <branch> -> git pull -u origin <branch_name (or) main>"
  echo "gpush <branch> -> git push -u origin <branch_name (or) main>"

  echo""
  echo "dots -> go to dotfiles directory"
  echo "savedots -> git commit and push all changes in dotfiles directory"
  echo "osbuild -> sudo nixos-rebuild switch --flake"
  echo "rebuild -> home-manager switch --flake"
  echo "sfu -> sudo nix flake update"
  echo "garbage -> sudo nix-collect-garbage -d"
  echo "news -> home-manager news --flake"
}



