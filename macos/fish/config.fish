######################################################
# Author: Shiva
# Updated on: 2025-Jan-09
# License: LOL
######################################################

########################### GENERAL ###########################
# CURSOR 
set fish_cursor_default block
# Set the cursor shapes for the different vi modes.
set fish_cursor_default block
set fish_cursor_insert block
# set fish_cursor_replace_one underscore blink
set fish_cursor_visual block

set -g fish_greeting ''

if status is-interactive
    # Commands to run in interactive sessions can go here
end

########################### PATHS ###########################

# Add locations to PATH
fish_add_path $HOME/.local/bin
fish_add_path /opt/nvim-linux64/bin
fish_add_path /snap/bin
fish_add_path $HOME/.cargo/bin
fish_add_path /usr/local/zig
fish_add_path $HOME/go/bin /usr/local/go/bin
fish_add_path /usr/local/go/bin
fish_add_path $HOME/.emacs.d/bin
fish_add_path $HOME/.config/emacs/bin


# Add fish scripts
# Automatically source all .fish scripts in ~/.config/fish/scripts/
# for script in $HOME/.config/fish/scripts/*.fish
#     source $script
# end

# Environment variables
# set -gx STARSHIP_CONFIG "$HOME/dotfiles/common/starship.toml"

# Terminal settings
set -gx TERM tmux-256color

# Go paths
set -gx GOROOT /usr/local/go
set -gx GOPATH $HOME/go

# NVM setup (requires fisher and nvm.fish plugin)
# Install with: fisher install jorgebucaran/nvm.fish

########################### ALIASES ###########################
# alias fdfind fd
# alias sk 'sudo kanata-start.sh'

# MacOS - g++ fix using alias (comment these on wsl/linux if runcpp wont work)
alias g++ 'g++-15'
alias gcc gcc-15

# Common
# alias cd="z"
alias vi="nvim"
alias ls="eza --icons -alh -L=1"
alias ll="eza --icons -alh -L=1"
alias l="eza --icons -alh -L=1"
alias files="explorer.exe"
alias y="yazi"
alias gg="lazygit"
alias ff="fastfetch"
alias c="clear"
alias gp="git pull"

# Git aliases
alias gaa="git add ."
alias gcmsg="git commit -m"
alias gst="git status ."
alias gitsave="gaa && gcmsg '.' && ggpush"
alias gpull="git pull"
alias gpush="git push"
alias gsave="gaa && gcmsg '.' && gpush"

# Python
alias python="python3.12"
alias py="python3.12"
alias pva="source ./venv/bin/activate"

# Other aliases
alias sourcefish="source $HOME/.config/fish/config.fish"
alias sudo="sudo "

# Set key bindings
# fish_vi_key_bindings
