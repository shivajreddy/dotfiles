# Powershell-7 dots

System Wide Powershell-7 profile

1 - It should be named 'profile.ps1'  
2 - It should be located at '$PSHOME'  
3- So make a sym link to the profile.ps1 file
```
New-Item -ItemType SymbolicLink -Path "$PSHOME\profile.ps1" -Target "~\dotfiles\windows\PowerShell\shiva.ps1"
```
NOTE: when you paste the above command, make sure just rewrite the target path, so that the '~' expands to 
the absolute path. If you will get an error like 
```text
.: Could not find a part of the path 'C:\Program Files\PowerShell\7\profile.ps1'.
```

User Lever Powershell profile
1 - It is named: 'Microsoft.PowerShell_profile.ps1'
2 - It should be located at '$PROFILE'
3 - so make a sym link to the Microsoft.PowerShell_profile.ps1 file in dotfiles
```
New-Item -ItemType SymbolicLink -Path "$PROFILE" -Target "~\dotfiles\windows\PowerShell\Microsoft.PowerShell_profile.ps1"
```


4 - reopen powershell
