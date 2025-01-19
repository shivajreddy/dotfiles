######################################################
# Author: Shiva
# Updated on: 2025-Jan-09
# License: LOL
######################################################

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
fish_add_path /usr/local/go/bin/

set -x C3_PATH /usr/local/lib/c3

# Add fish scripts
# Automatically source all .fish scripts in ~/.config/fish/scripts/
for script in $HOME/.config/fish/scripts/*.fish
    source $script
end



# Environment variables
set -gx STARSHIP_CONFIG "$HOME/dotfiles/common/starship.toml"
set -gx BROWSER '/mnt/c/Program\ Files/Google/Chrome/Application/chrome.exe'

# Qt5 variables
set -gx QT_QPA_PLATFORMTHEME qt5ct

# Library paths for c/c++
set -gx LD_LIBRARY_PATH /usr/local/lib:$LD_LIBRARY_PATH

# HASKELL
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin /home/smpl/.ghcup/bin $PATH # ghcup-env

# Terminal settings
set -gx TERM tmux-256color

# Go paths
set -gx GOROOT /usr/local/go
set -gx GOPATH $HOME/go

# NVM setup (requires fisher and nvm.fish plugin)
# Install with: fisher install jorgebucaran/nvm.fish

########################### ALIASES ###########################

# Common
# alias cd="z"
alias vi="nvim"
alias ls="eza --icons -l -T -L=1"
alias l="ls -l"
alias ll="ls -alh"
alias files="explorer.exe"
alias y="yazi"
alias ff="fastfetch"

# Git aliases
alias gaa="git add ."
alias gcmsg="git commit -m"
alias gst="git status ."
alias gitsave="gaa && gcmsg '.' && ggpush"
alias gpull="git pull"
alias gpush="git push"
alias gsave="gaa && gcmsg '.' && gpush"

# Git clone function
function gclone
    if test (count $argv) -ne 2
        echo "Usage: gclone <github-username> <repository-name>"
        return 1
    end
    set username $argv[1]
    set repo $argv[2]
    git clone "https://github.com/$username/$repo.git"
end

# Python
alias python="python3.12"
alias py="python3.12"
alias pva="source ./venv/bin/activate.fish"
# alias pva="source ./venv/bin/activate"

# Python watch function
function pywatch
    find . -name $argv[1] | entr -c sh -c "python3 $argv[2]"
end

# Linux Related
alias fd="fdfind"
# alias time='/usr/bin/time -f "\nreal\t%E\nuser\t%U\nsys\t%S"'

# WSL
alias pwsh="pwsh.exe -nologo"

# Zellij related aliases
alias zc="zellij action new-tab -l c"
alias zcpp="zellij action new-tab -l cpp"
alias zrust="zellij action new-tab -l rust"
alias zrustv="zellij action new-tab -l rust_vertical"
alias zpy="zellij action new-tab -l python"
alias zpyv="zellij action new-tab -l python_vertical"

# Other aliases
alias sourcefish="source $HOME/.config/fish/config.fish"
alias sudo="sudo "

########################### PLUGINS ###########################

# For plugins, you'll need to install Fisher (plugin manager for fish)
# curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

# Required plugins (install with fisher):
# fisher install kidonng/zoxide.fish
# fisher install meaningful-ooo/sponge # for command correction
# fisher install jorgebucaran/autopair.fish # for auto-pairing brackets
# fisher install IlanCosman/tide@v5 # a theme (alternative to starship)
# fisher install PatrickF1/fzf.fish # for fzf integration
# fzf --fish | source

# Starship prompt initialization
starship init fish | source

# Set key bindings
fish_vi_key_bindings

########################### OTHER ###########################
# Word characters (fish handles this differently from zsh)
# Fish has its own word separation logic that's generally more intuitive

# Spicetify path
fish_add_path $HOME/.spicetify

