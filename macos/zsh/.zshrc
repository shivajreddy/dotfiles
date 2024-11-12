#-----------------------------------------------------------|
#   Author: Shiva                                           |
#   Updated on: 2024-July-17                                |
#   License: LOL                                            |
#-----------------------------------------------------------|

########################### PATHS ###########################
############# Linux #############

########## WINDOWS-WSL ##########
# # Paths of my shell scripts
# # this seems to be not working
# export PATH="$HOME/.config/zsh/scripts:$PATH"
#
# # for getting colors properly on tmux
# export TERM=tmux-256color

############# MacOS #############
PATH="/Library/Frameworks/SDL2.framework/./:${PATH}"
export PATH

LIBRARY_PATH="/Library/Frameworks/SDL2.framework/:${LIBRARY_PATH}"
export LIBRARY_PATH

CPLUS_INCLUDE_PATH="/Library/Frameworks/SDL2.framework/Headers/:$CPLUS_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH

CPATH="/Library/Frameworks/SDL2.framework/Headers:$CPATH"
export CPATH

LIBRARY_PATH="/Library/Frameworks/SDL2.framework:$LIBRARY_PATH"
export LIBRARY_PATH


########################### PLUGINS ###########################
# zoxide - z (cd replacement)
eval "$(zoxide init zsh)"
# starship prompt
eval "$(starship init zsh)"
# auto suggestions
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# syntax highlighting
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# echo "source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
# source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh




########################### ALIASES ###########################
# Common
# alias cd="z"
alias vi="nvim"
alias ls="eza --icons -l -T -L=1"
alias l="ls -l"
alias ll="ls -alh"
alias files="open"
# alias files="explorer.exe"
alias gaa="git add ."
alias gcmsg="git commit -m"
alias gst="git status ."
alias gitsave="gaa && gcmsg '.' && ggpush"
alias gpull="git pull"
alias gpush="git push"
alias python="python3"
alias py="python3"
alias pva="source ./venv/bin/activate"
alias ff="fastfetch"

pywatch() {
  find . -name "$1" | entr -c sh -c "python3 $2"
}

# Zellij related aliases
alias zc="zellij action new-tab -l c"
alias zcpp="zellij action new-tab -l cpp"
alias zrust="zellij action new-tab -l rust"
alias zpy="zellij action new-tab -l python"

# Shell scripts
alias show="$HOME/.config/zsh/scripts/show.sh"
alias todo="$HOME/.config/zsh/scripts/todo.sh"

# Shell scripts
source ~/.config/zsh/scripts/cargo.sh
source ~/.config/zsh/scripts/show.sh
source ~/.config/zsh/scripts/todo.sh

# other/common/misc.
alias sourcezsh="source $HOME/.config/zsh/.zshrc"
# Enable aliases to be sudo’ed
alias sudo='sudo '

