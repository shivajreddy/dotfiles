

## Symlink on windows
```bash
New-Item -ItemType SymbolicLink -d -Path "$env:LOCALAPPDATA\nvim" -Target "\\wsl`$\Ubuntu\home\shiva\dotfiles\common\nvim\"
```


