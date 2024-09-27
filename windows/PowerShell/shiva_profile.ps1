<#

1 - First create a profile for the windows user if not already using the following
command
(https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_profiles?view=powershell-7.4)

if (!(Test-Path -Path $PROFILE)) {
  New-Item -ItemType File -Path $PROFILE -Force
}

2 - open the profile that you created, use the 
echo $Profile
to see where your user profile file lives at

3 - Now manually copy the contents of dotfiles/windows/powershell/shiva_profile.ps1
and paste into the windows user's profile file

4 - reopen powershell

#>

Set-Location ~


# ####	ZOXIDE    ####
Set-Alias z zoxide
Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$ENV:STARSHIP_CONFIG = "\\wsl.localhost\Ubuntu\home\shiva\dotfiles\windows\starship.toml"
Invoke-Expression (&starship init powershell)


# ####	ALIASES   ####
Set-Alias -Name vi -Value nvim

Set-Alias -Name python -Value py
Set-Alias -Name python3 -Value py

Function ListEza {eza --icons -T -L=1}
New-Alias -Force -Name ls -Value ListEza

Function ListPermissionsEza {eza --icons -l -T -L=1}
New-Alias -Force -Name ll -Value ListPermissionsEza

Set-Alias l ll

# ####	MISC.    ####
# when using dir, hide the ugly text background color
$PSStyle.FileInfo.Directory = ""





