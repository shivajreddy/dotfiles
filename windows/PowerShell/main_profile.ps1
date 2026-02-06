# Use this as system profile, so sym link this file to $PSHOME/profile.ps1
# Note: Removed "Set-Location ~" to allow WezTerm to spawn tabs in current directory

# Turn of the update powershell statement when i open powershell
$env:POWERSHELL_UPDATECHECK = 'Off'

# ####	ZOXIDE    ####
# Set-Alias z zoxide
# Invoke-Expression (& { (zoxide init powershell | Out-String) })


# ####	STARSHIP    ####
# Location of starship configuration
$Env:STARSHIP_CONFIG = "$HOME\dotfiles\windows\starship.toml"
Invoke-Expression (&starship init powershell)

# ####	OSC 7 - Tell WezTerm the current directory    ####
# This must come AFTER starship init since starship overrides the prompt function
# This enables WezTerm to know your CWD for spawning new tabs/panes
$starshipPrompt = $function:prompt
function prompt {
    # First emit OSC 7 with current directory
    $p = $executionContext.SessionState.Path.CurrentLocation.Path
    $osc7 = "`e]7;file://localhost/$($p -replace '\\', '/')`e\"
    [Console]::Write($osc7)
    # Then call starship prompt
    & $starshipPrompt
}


# ####	ALIASES   ####
Set-Alias -Name vi -Value nvim
Set-Alias -Name gg -Value lazygit
Set-Alias -Name y -Value yazi
Set-Alias -Name c -Value cls
Set-Alias -Name ff -Value fastfetch


Set-Alias -Name cc -Value claude
Set-Alias -Name oo -Value opencode

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


# ####	FUNCTIONS    ####
# Delete files with reserved names (nul, con, aux, etc.) that AI tools sometimes create
function Remove-NulFile {
    param(
        [Parameter(Mandatory=$true)]
        [string]$FilePath
    )
    
    $fileName = Split-Path -Leaf $FilePath
    $directory = Split-Path -Parent $FilePath
    
    # If no directory specified, use current directory
    if ([string]::IsNullOrEmpty($directory)) {
        $directory = (Get-Location).Path
    } else {
        $directory = (Resolve-Path $directory).Path
    }
    
    $fullPath = Join-Path $directory $fileName
    $tempName = "deletefile_$([System.Guid]::NewGuid().ToString('N').Substring(0,8)).txt"
    $tempPath = Join-Path $directory $tempName
    
    # Use \\.\<path> syntax to access the reserved filename
    $dosDevicePath = "\\.\$fullPath"
    
    cmd /c "rename `"$dosDevicePath`" `"$tempName`" && del `"$tempPath`""
    
    if (Test-Path $tempPath) {
        Write-Error "Failed to delete: $FilePath"
    } else {
        Write-Host "Successfully deleted: $FilePath"
    }
}
