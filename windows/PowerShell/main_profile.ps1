# Use this as system profile, so sym link this file to $PSHOME/profile.ps1
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
Set-Alias -Name ff -Value fastfetch
Function wu {
    winget upgrade --all --include-unknown
}

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
$PSStyle.FileInfo.Directory = ""

# Allow Ctrl+D to exit, instead of typing as ^D
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteCharOrExit


# Helper function to reload environment variables and PowerShell profile without opening a new session
Function rrr {
    # Save the current directory location so we can return to it after reloading
    $currentLocation = Get-Location

    # Rebuild the PATH environment variable by combining System PATH and User PATH
    # This picks up any changes made to PATH in System Properties > Environment Variables
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    Write-Host "Environment variables reloaded" -ForegroundColor Green

    # Re-source the PowerShell profile to load any config changes
    # This re-executes all the commands in the profile (aliases, functions, etc.)
    . "$PSHOME\profile.ps1"
    Write-Host "Re-Sourced PowerShell Config" -ForegroundColor Green

    # Return to the directory we were in before reloading
    # (The profile has "Set-Location ~" which would otherwise change our location)
    Set-Location $currentLocation
    Write-Host "Re-Located to the current directory" -ForegroundColor Green
}
