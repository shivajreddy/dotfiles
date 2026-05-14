# =============================================================================
#  POWERSHELL PROFILE
#
#  System-wide  (all users):   New-Item -ItemType SymbolicLink -Path "$PSHOME\profile.ps1" -Target "$HOME\dotfiles\windows\PowerShell\ulteig_profile.ps1"
#  Current user only:          New-Item -ItemType SymbolicLink -Path "$PROFILE" -Target "$HOME\dotfiles\windows\PowerShell\ulteig_profile.ps1"
#
#  Note: requires admin or Developer Mode enabled for symlinks on Windows.
# =============================================================================


# =============================================================================
#  SHELL CONFIG
#  Environment variables and terminal behavior - configure first so everything
#  that follows (prompt, aliases, tools) inherits the right settings.
# =============================================================================

# Suppress the "update PowerShell" banner on startup
$env:POWERSHELL_UPDATECHECK = 'Off'

# Prepend npm global bin to PATH
$env:PATH = "$HOME\AppData\Roaming\npm;$env:PATH"
# temporary until i install nvim propery
$env:PATH = "$HOME\bin\nvim-win64\bin\;$env:PATH" 

# Starship config path (must be set before starship init below)
$Env:STARSHIP_CONFIG = "$HOME\dotfiles\windows\starship.toml"

# Yazi: path to file(1) binary, required for mime-type detection on Windows
$env:YAZI_FILE_ONE = "$env:LOCALAPPDATA\Programs\Git\usr\bin\file.exe"

# Hide the colored background on directory entries in 'dir'
$PSStyle.FileInfo.Directory = ""

# Allow Ctrl+D to exit the shell (Unix-style)
Set-PSReadLineKeyHandler -Key Ctrl+d -Function DeleteCharOrExit


# =============================================================================
#  PROMPT  (Starship + OSC 7 for WezTerm CWD)
#  OSC 7 must be injected AFTER starship init — starship overrides the prompt
#  function, so we wrap it. This lets WezTerm spawn new tabs in the current dir.
# =============================================================================

Invoke-Expression (&starship init powershell)

$starshipPrompt = $function:prompt
function prompt {
    $p = $executionContext.SessionState.Path.CurrentLocation.Path
    $osc7 = "`e]7;file://localhost/$($p -replace '\\', '/')`e\"
    [Console]::Write($osc7)
    & $starshipPrompt
}


# =============================================================================
#  NAVIGATION
# =============================================================================

function dots  { Set-Location ~/dotfiles }
function roam  { Set-Location $ENV:APPDATA }
function local { Set-Location $ENV:LOCALAPPDATA }


# =============================================================================
#  GIT
# =============================================================================

# Remove conflicting PowerShell built-in aliases
Remove-Item alias:gl  -Force -ErrorAction SilentlyContinue
Remove-Item alias:gp  -Force -ErrorAction SilentlyContinue
Remove-Item alias:gcm -Force -ErrorAction SilentlyContinue

function g    { git @args }
function ga   { git add @args }
function gaa  { git add . }
function gb   { git branch --show-current }
function gcl  { git clone @args }
function gch  { git checkout -b @args }
function gco  { param([string]$msg) git commit -m "$msg" }
function gcp  { git cherry-pick @args --no-commit }
function gcm {
    param(
        [Parameter(ValueFromRemainingArguments = $true)]
        [string[]]$msg
    )
    if (-not $msg) { Write-Error "Commit message required"; return }
    git commit -m ($msg -join " ")
}
function gbd     { git branch -D @args }
function gd      { git diff @args }
function gl      { git log @args }
function gp      { git pull @args }
function gpu     { git push @args }
function gs      { git status }
function gsc     { git stash clear }
function gsa     { git stash apply }
function git-sha { git rev-parse HEAD }
function greset  { param([string]$commit) git reset --soft $commit }


# =============================================================================
#  TOOLS
# =============================================================================

Set-Alias -Name vi -Value nvim
Set-Alias -Name gg -Value lazygit
Set-Alias -Name y  -Value yazi
Set-Alias -Name c  -Value cls
Set-Alias -Name ff -Value fastfetch
Set-Alias -Name fp -Value fpilot
Set-Alias -Name cc -Value claude
Set-Alias -Name o  -Value opencode

function oo { opencode -c }    # open with context


# =============================================================================
#  FILE LISTING  (eza)
# =============================================================================

function l { eza --icons -l -a $args }

function ListPermissionsEza { eza --icons -l -T -L=1 }
New-Alias -Force -Name ll -Value ListPermissionsEza


# =============================================================================
#  LANGUAGES
# =============================================================================

# Python — alias 'python' / 'python3' to the Windows py launcher
Set-Alias -Name python  -Value py
Set-Alias -Name python3 -Value py

# Rust / Cargo
function Invoke-CargoRun { cargo run @args }
Set-Alias cr Invoke-CargoRun


# =============================================================================
#  UTILITIES
# =============================================================================

# Resolve which binary is on PATH (replaces the built-in which-less experience)
function which { where.exe $args }

# Elevate and upgrade all winget packages
function wu { winget upgrade --all --include-unknown }

# Delete the reserved 'nul' device file from a directory
# Usage: nul .   or   nul C:\some\path
function nul {
    param([string]$Directory = ".")
    $dir      = (Resolve-Path $Directory).Path
    $fullPath = Join-Path $dir "nul"
    $tempName = "deleteme_$([System.Guid]::NewGuid().ToString('N').Substring(0,8)).txt"
    $tempPath = Join-Path $dir $tempName

    cmd /c "rename `"\\.\$fullPath`" `"$tempName`" && del `"$tempPath`""

    if (Test-Path $tempPath) {
        Write-Error "Failed to delete nul in: $dir"
    } else {
        Write-Host "Deleted nul in: $dir"
    }
}

# Run a command, stream its output, then copy the result to the clipboard
# Usage: iac cargo build   or   iac git log --oneline
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


# =============================================================================
#  DISABLED / EXPERIMENTAL
# =============================================================================

# Zoxide (z) — uncomment to enable
# Set-Alias z zoxide
# Invoke-Expression (& { (zoxide init powershell | Out-String) })

# Eza tree view — uncomment to enable
# function ListEza { eza --icons -T -L=1 }
