# Load .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

export SHELL=/bin/bash
export PATH="$(brew --prefix llvm)/bin:$PATH"
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"
export PATH="/usr/local/opt/openvpn/sbin:$PATH"
