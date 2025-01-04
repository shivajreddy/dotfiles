#-----------------------------------------------------------|
#   Author: Shiva                                           |
#   Updated on: 2024-July-17                                |
#   License: LOL                                            |
#-----------------------------------------------------------|


########################### PATHS ###########################

# Add locations to 'Path' variable
export PATH="$HOME/.local/bin:$PATH"

export PATH="$PATH:/opt/nvim-linux64/bin"

export PATH=$PATH:/snap/bin

export STARSHIP_CONFIG="$HOME/dotfiles/common/starship.toml"

export BROWSER='/mnt/c/Program\ Files/Google/Chrome/Application/chrome.exe'
# export BROWSER='/mnt/c/Program\ Files/BraveSoftware/Brave-Browser/Application/brave.exe'

# Add my shell scripts to PATH
export PATH="$HOME/.config/zsh/scripts:$PATH"

# Add Cargo to path
export PATH="$HOME/.cargo/bin:$PATH"

export PATH=$PATH:/usr/local/zig

# qt5 variables
export QT_QPA_PLATFORMTHEME=qt5ct
# export QT_QUICK_CONTROLS_STYLE=Material
# export QT_STYLE_OVERRIDE=qt6ct

# Library paths for c/c++
LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# for getting colors properly on tmux
export TERM=tmux-256color

# Go paths
export GOROOT=/usr/local/go
export GOPATH=$HOME/go
export PATH=$HOME/go/bin:/usr/local/go/bin:$PATH
export PATH="/usr/local/go/bin/:$PATH"

# NVM & NPM paths
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion




########################### PLUGINS ###########################

# lf
LFCD=~/dotfiles/wsl/lf/lfcd.sh
if [ -f "$LFCD" ]; then
    source "$LFCD"
fi
# auto suggestions
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# syntax highlighting
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zoxide - z (cd replacement)
eval "$(zoxide init zsh)"

# starship prompt
eval "$(starship init zsh)"


########################### ALIASES ###########################
# Common
# alias cd="z"
alias vi="nvim"
alias ls="eza --icons -l -T -L=1"
alias l="ls -l"
alias ll="ls -alh"
alias files="explorer.exe"

# Git 
alias gaa="git add ."
alias gcmsg="git commit -m"
alias gst="git status ."
alias gitsave="gaa && gcmsg '.' && ggpush"
alias gpull="git pull"
alias gpush="git push"
alias gsave="gaa && gcmsg '.' && gpush"
gclone() {  # Function to clone a GitHub repository
    if [ "$#" -ne 2 ]; then
        echo "Usage: gclone <github-username> <repository-name>"
        return 1
    fi
    local username="$1"
    local repo="$2"
    git clone "https://github.com/${username}/${repo}.git"
}

# Python
alias python="python3.12"
alias py="python3.12"
alias pva="source ./venv/bin/activate"
# Use entr to watch. 
# Example: pywatch some_file.py some_file.py
# Example: pywatch "*.py" ./src/company/microsoft/course_schedule_II.py
pywatch() {
  find . -name "$1" | entr -c sh -c "python3 $2"
}

# CHATGPT
export OPENAI_KEY=

# Linux Related
alias fd="fdfind"

# - formats nicely when using the `time` command
alias time='/usr/bin/time -f "\nreal\t%E\nuser\t%U\nsys\t%S"'

# For wsl stuff
alias pwsh="pwsh.exe -nologo"
# alias pwsh="powershell.exe -nolog"

# Zellij related aliases
alias zc="zellij action new-tab -l c"
alias zcpp="zellij action new-tab -l cpp"
alias zrust="zellij action new-tab -l rust"
alias zrustv="zellij action new-tab -l rust_vertical"
alias zpy="zellij action new-tab -l python"
alias zpyv="zellij action new-tab -l python_vertical"
# alias znc="zellij action new-tab -l shiva_c"
# alias zncp="zellij action new-tab -l shiva_cpp"
# alias znr="zellij action new-tab -l shiva_rust"


# Shell scripts
source ~/.config/zsh/scripts/wezterm.sh
source ~/.config/zsh/scripts/cargo.sh
source ~/.config/zsh/scripts/show.sh
source ~/.config/zsh/scripts/todo.sh

# other/common/misc.
alias sourcezsh="source $HOME/.config/zsh/.zshrc"
# Enable aliases to be sudo’ed
alias sudo='sudo '

########################### OTHER ###########################
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'



export PATH=$PATH:/home/smpl/.spicetify
