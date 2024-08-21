# This will remove the background color for directories in 'ls'
$PSStyle.FileInfo.Directory = ""

Set-Alias -Name vi -Value nvim
Set-Alias -Name files -Value "explorer.exe"


try {
    Remove-Item Alias:clear -ErrorAction Stop
} catch {
    Write-Host "Could not remove the existing alias 'clear'. It might have the AllScope option."
}
Set-Alias -Name clear -Value cls


# # Remove existing alias or function for ls
if (Test-Path Function:\ls) {
    Remove-Item Function:\ls
}

# Define custom ls function
function l {
    eza --icons -l -T -L=1
}
function ls {
    eza --icons -l -T -L=1
}

# Define custom l function
function ll {
    eza --icons -al -T -L=1
}


Invoke-Expression (&starship init powershell)

# Import the Chocolatey Profile that contains the necessary code to enable
# tab-completions to function for `choco`.
# Be aware that if you are missing these lines from your profile, tab completion
# for `choco` will not function.
# See https://ch0.co/tab-completion for details.
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

