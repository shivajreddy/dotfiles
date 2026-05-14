
glazewm looks for the config in the path C:\Users\<user>\.glzr

NOTE: we use the dotfiles repo on windows, instead of wsl repo, because,
      some of the config in glazem/zebar when using path, it cant look for wsl 
      directory, there is no way of using wsl folder, for this reason just use 
      dotfiles folder on windows

Example: make sure to check username/paths
```
New-Item -ItemType SymbolicLink -Path "C:\Users\shiva\.glzr" -Target "C:\Users\shiva\dotfiles\windows\glzr\" 
```

