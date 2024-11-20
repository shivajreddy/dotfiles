<#

System Wide Powershell-7 profile

1 - It should be named 'profile.ps1'
2 - It should be located at '$PSHOME'
3- So make a sym link to the profile.ps1 file 
New-Item -ItemType SymbolicLink -Path "$PSHOME\profile.ps1" -Target "\\wsl`$\Debian\home\shiva\dotfiles\windows\PowerShell\profile.ps1"
4 - reopen powershell

#>

Set-Location ~


# ####	ZOXIDE    ####
# Set-Alias z zoxide
# Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$ENV:STARSHIP_CONFIG = "\\wsl.localhost\Ubuntu\home\shiva\dotfiles\windows\starship.toml"
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

