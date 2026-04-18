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
Set-Alias -Name fp -Value fpilot
function which {
    where.exe $args
}
function dots {
    Set-Location ~/dotfiles
}

# GIT Aliases
# Remove conflicting built-in aliases
Remove-Item alias:gl -Force -ErrorAction SilentlyContinue
Remove-Item alias:gp -Force -ErrorAction SilentlyContinue
# --- Git functions (PowerShell-native) ---
function g   { git @args }
function ga  { git add @args }           # better than hardcoding '.'
function gaa  { git add . }           # better than hardcoding '.'
function gb  { git branch --show-current }
function gcl { git clone @args }
function gch { git checkout -b @args }
function gco { param([string]$msg) git commit -m "$msg" }
function gcp { git cherry-pick @args --no-commit }
function gbd { git branch -D @args }
function gd  { git diff @args }
function gl  { git log @args }
function gp  { git pull @args }
function gpu { git push @args }
function gs  { git status }
function gsc { git stash clear }
function gsa { git stash apply }
function git-sha { git rev-parse HEAD }
function greset {
    param([string]$commit)
    git reset --soft $commit
}

# LOCALAPPDATA & ROAMING
Function roam {
    Set-Location $ENV:APPDATA
}
Function local {
    Set-Location $ENV:LOCALAPPDATA
}

Set-Alias -Name cc -Value claude
# Set-Alias -Name oo -Value opencode
function oo {
        opencode -c
}
Set-Alias -Name o -Value opencode

Function wu {
    sudo winget upgrade --all --include-unknown
}

Set-Alias -Name python -Value py
Set-Alias -Name python3 -Value py

# Function ListEza {eza --icons -T -L=1}
Function l {
    eza --icons -l -a $args
}


Function ListPermissionsEza {eza --icons -l -T -L=1}
New-Alias -Force -Name ll -Value ListPermissionsEza

# ####	MISC.    ####
# when using dir, hide the ugly text background color
$PSStyle.FileInfo.Directory = ""

# Allow Ctrl+D to exit, instead of typing as ^D
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteCharOrExit


# ####	FUNCTIONS    ####
# Delete 'nul' reserved file in a directory. Usage: nul .  or  nul C:\some\path
function nul {
    param(
        [string]$Directory = "."
    )
    
    $dir = (Resolve-Path $Directory).Path
    $fullPath = Join-Path $dir "nul"
    $tempName = "deleteme_$([System.Guid]::NewGuid().ToString('N').Substring(0,8)).txt"
    $tempPath = Join-Path $dir $tempName
    
    $dosDevicePath = "\\.\$fullPath"
    cmd /c "rename `"$dosDevicePath`" `"$tempName`" && del `"$tempPath`""
    
    if (Test-Path $tempPath) {
        Write-Error "Failed to delete nul in: $dir"
    } else {
        Write-Host "Deleted nul in: $dir"
    }
}

# Invoke the cli command and then copy the result into clipboard
function Invoke-And-Copy {
    param(
        [Parameter(Mandatory, ValueFromRemainingArguments)]
        [string[]]$Command
    )

    $exe, $rest = $Command
    & $exe @rest 2>&1 | Tee-Object -Variable output
    $output | Set-Clipboard
    Write-Host "`n[Copied to clipboard]" -ForegroundColor Green
}
Set-Alias iac Invoke-And-Copy


# RUST Aliases
function Invoke-CargoRun { cargo run @args }
Set-Alias cr Invoke-CargoRun

