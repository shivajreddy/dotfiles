# Powershell-7 dots

System Wide Powershell-7 profile

1 - It should be named 'profile.ps1'  
2 - It should be located at '$PSHOME'  
3- So make a sym link to the profile.ps1 file

```
New-Item -ItemType SymbolicLink -Path "$PSHOME\profile.ps1" -Target "\\wsl`$\Debian\home\shiva\dotfiles\windows\PowerShell\profile.ps1"
```
```
New-Item -ItemType SymbolicLink -Path "$PSHOME\profile.ps1" -Target "\\wsl`$\Ubuntu\home\smpl\dotfiles\windows\PowerShell\profile.ps1"
```

4 - reopen powershell
