## Add locations to 'Path' variable
export PATH="$HOME/.local/bin:$PATH"

export TERM=tmux-256color
# bindkey '^ ' autosuggest-accept


## PLUGINS
# auto suggestions
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
# syntax highlighting
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zoxide - z (cd replacement)
eval "$(zoxide init zsh)"

# starship prompt
eval "$(starship init zsh)"

# aliases
alias cd="z"
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
alias py="python3"

