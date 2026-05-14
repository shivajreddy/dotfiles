######################################################
# Author: Shiva
# Updated on: 2025-Jan-09
######################################################

########################### GENERAL ###########################

# Disable bash greeting (if any)
# Bash doesn't have a greeting by default

########################### PATHS ###########################

# Add locations to PATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="/opt/nvim-linux64/bin:$PATH"
export PATH="/snap/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/zig:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/.spicetify:$PATH"

# Source all bash scripts in ~/.config/bash/scripts/ (if they exist)
if [ -d "$HOME/.config/bash/scripts" ]; then
    for script in "$HOME/.config/bash/scripts"/*.sh; do
        [ -f "$script" ] && source "$script"
    done
fi

# Environment variables
export BROWSER='/mnt/c/Program\ Files/Google/Chrome/Application/chrome.exe'
export QT_QPA_PLATFORMTHEME=qt5ct
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export TERM=tmux-256color
export GOROOT=/usr/local/go
export GOPATH=$HOME/go

########################### ALIASES ###########################

# MacOS - g++ fix using alias
alias g++='g++-15'
alias gcc='gcc-15'

# Common
alias vi='nvim'
alias ls='eza --icons -alh -L=1'
alias ll='eza --icons -alh -L=1'
alias l='eza --icons -alh -L=1'
alias files='explorer.exe'
alias y='yazi'
alias gg='lazygit'
alias ff='fastfetch'
alias c='clear'
alias gp='git pull'

# Git aliases
alias gaa='git add .'
alias gcmsg='git commit -m'
alias gst='git status .'
alias gpull='git pull'
alias gpush='git push'
alias gsave='gaa && gcmsg "." && gpush'
alias gitsave='gaa && gcmsg "." && gpush'

# Python
alias python='python3.12'
alias py='python3.12'
alias pva='source ./venv/bin/activate'

# Linux Related
alias fd='fdfind'

# WSL
alias pwsh='pwsh.exe -nologo'

# Zellij related aliases
alias zc='zellij action new-tab -l c'
alias zcpp='zellij action new-tab -l cpp'
alias zrust='zellij action new-tab -l rust'
alias zrustv='zellij action new-tab -l rust_vertical'
alias zpy='zellij action new-tab -l python'
alias zpyv='zellij action new-tab -l python_vertical'

# Other aliases
alias sourcebash='source ~/.bashrc'
alias sudo='sudo '

########################### FUNCTIONS ###########################

# Python watch function
pywatch() {
    find . -name "$1" | entr -c sh -c "python3 $2"
}

########################### PLUGINS/TOOLS ###########################

# Vi mode for bash (equivalent to fish_vi_key_bindings)
set -o vi

# Starship prompt
eval "$(starship init bash)"

# Zoxide initialization (if installed)
if command -v zoxide &>/dev/null; then
    eval "$(zoxide init bash)"
    # alias cd='z'  # Uncomment if you want cd to use zoxide
fi

# FZF initialization (if installed)
if command -v fzf &>/dev/null; then
    eval "$(fzf --bash)"
fi

# NVM setup (if installed)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"

########################### OTHER ###########################

# Silence macOS zsh deprecation warning
export BASH_SILENCE_DEPRECATION_WARNING=1

# Better history settings
export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend

# Better directory navigation
# shopt -s autocd  # cd by just typing directory name
# shopt -s cdspell  # autocorrect typos in cd
