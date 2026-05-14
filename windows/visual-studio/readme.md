
### Make a symlink to vimrc file from the home folder

In Powershell
1. Go to 'cd ~'
2. Make a symlink
```
New-Item -ItemType SymbolicLink -Path "C:\LinkFolder\MyLink.txt" -Target "C:\OriginalFolder\OriginalFile.txt"
```

Example:
```
New-Item -ItemType SymbolicLink -Path "~\.vimrc" -Target "C:\Users\smpl\dotfiles\windows\visual-studio\.vimrc"
```

