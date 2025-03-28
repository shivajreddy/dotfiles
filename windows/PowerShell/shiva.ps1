# echo "This is user profile"
Set-Location ~

# Turn of the update powershell statement when i open powershell
$env:POWERSHELL_UPDATECHECK = 'Off'

# ####	ZOXIDE    ####
# Set-Alias z zoxide
# Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$Env:STARSHIP_CONFIG = "C:\Users\sreddy\dotfiles\windows\starship.toml"
# 2. Permanently set for the current user
[Environment]::SetEnvironmentVariable("STARSHIP_CONFIG", "C:\Users\sreddy\dotfiles\windows\starship.toml", "User")
# 3. Permanently set for the machine (requires admin rights)
[Environment]::SetEnvironmentVariable("STARSHIP_CONFIG", "C:\Users\sreddy\dotfiles\windows\starship.toml", "Machine")

Invoke-Expression (&starship init powershell)


# ####	ALIASES   ####
Set-Alias -Name vi -Value nvim

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
