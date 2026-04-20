# Linking windows-terminal config to dotfiles
- [useful article](https://helgeklein.com/blog/changing-location-windows-terminal-settings-file/)
- NOTE: Make sure Windows-Terminal is closed before running
- to make sure the the path set for $LOCALSTATE is still the same at the time  
  of making this script, open windows-terminal > settings > Open Json file then  
  look at the path of this json file

```ps1
# Target path
$TARGETPATH = "$HOME\dotfiles\windows\terminal"

# Original LocalState path
$LOCALSTATE = "$env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState"

# Remove existing folder (if it exists)
if (Test-Path $LOCALSTATE) {
    Remove-Item $LOCALSTATE -Recurse -Force
}

# Create symbolic link
New-Item -ItemType SymbolicLink -Path $LOCALSTATE -Target $TARGETPATH
```

