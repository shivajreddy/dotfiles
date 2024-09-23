
# ### Set where the profile file for pwsh exists
$PROFILE = "C:\Program Files (x86)\PowerShell\7\Profile.ps1"


# ####	ZOXIDE    ####
Set-Alias z zoxide
Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$ENV:STARSHIP_CONFIG = "\\wsl.localhost\Ubuntu\home\shiva\dotfiles\common\starship\starship.toml"
Invoke-Expression (&starship init powershell)


# ####	ALIASES   ####
Set-Alias -Name vi -Value nvim
Set-Alias nf neofetch

Function ListEza {eza --icons -T -L=1}
New-Alias -Force -Name ls -Value ListEza

Function ListPermissionsEza {eza --icons -l -T -L=1}
New-Alias -Force -Name ll -Value ListPermissionsEza

Set-Alias l ll

# ####	MISC.    ####
# when using dir, hide the ugly text background color
$PSStyle.FileInfo.Directory = ""



