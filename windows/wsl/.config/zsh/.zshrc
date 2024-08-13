#-----------------------------------------------------------|
#   Author: Shiva                                           |
#   Updated on: 2024-July-17                                |
#   License: LOL                                            |
#-----------------------------------------------------------|

########################### PATHS ###########################
# Add locations to 'Path' variable
export PATH="$HOME/.local/bin:$PATH"

export BROWSER='/mnt/c/Program\ Files/BraveSoftware/Brave-Browser/Application/brave.exe'


# Paths of my shell scripts
# this seems to be not working
export PATH="$HOME/.config/zsh/scripts:$PATH"

# for getting colors properly on tmux
export TERM=tmux-256color

########################### ENV VARIABLES ###########################
. "$HOME/.cargo/env"


########################### PLUGINS ###########################
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
alias gaa="git add ."
alias gcmsg="git commit -m"
alias gst="git status ."
alias gitsave="gaa && gcmsg '.' && ggpush"
alias gpull="git pull"
alias gpush="git push"
alias python="python3"
alias py="python3"
alias pva="source ./venv/bin/activate"

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
source ~/.config/zsh/scripts/cargo.sh
source ~/.config/zsh/scripts/show.sh
source ~/.config/zsh/scripts/todo.sh

# other/common/misc.
alias sourcezsh="source $HOME/.config/zsh/.zshrc"
# Enable aliases to be sudo’ed
alias sudo='sudo '


