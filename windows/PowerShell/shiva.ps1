# echo "This is user profile"
Set-Location ~

# Turn of the update powershell statement when i open powershell
$env:POWERSHELL_UPDATECHECK = 'Off'

# ####	ZOXIDE    ####
# Set-Alias z zoxide
# Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$Env:STARSHIP_CONFIG = "$HOME\dotfiles\windows\starship.toml"

Invoke-Expression (&starship init powershell)


# ####	ALIASES   ####
Set-Alias -Name vi -Value nvim
Set-Alias -Name gg -Value lazygit
Set-Alias -Name y -Value yazi
Set-Alias -Name c -Value cls

Set-Alias -Name python -Value py
Set-Alias -Name python3 -Value py

# Function ListEza {eza --icons -T -L=1}
# New-Alias -Force -Name ls -Value ListEza
Function ls {
    eza --icons -l $args
}


Function ListPermissionsEza {eza --icons -l -T -L=1}
New-Alias -Force -Name ll -Value ListPermissionsEza

Set-Alias l ll

# ####	MISC.    ####
# when using dir, hide the ugly text background color
# $PSStyle.FileInfo.Directory = ""
