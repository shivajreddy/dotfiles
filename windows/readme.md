# Setting up new Windows machine

### WSL
- install wsl and ubuntu
- install wezterm, symlink wezterm folder
  mklink /D "C:\Users\sreddy\.config\wezterm" "\\wsl$\Ubuntu\home\shiva\dotfiles\common\wezterm"
- install lazygit
    https://github.com/jesseduffield/lazygit?tab=readme-ov-file#ubuntu
- install build-essential, cmake
- install node, nvm, npm
- install python
- install golang
    https://www.cherryservers.com/blog/install-go-ubuntu
- 

### Windows
- install berkeley mono, iosevka, jetbrains mono font
- install firefox, symlink chrome folder
- Terminal
- powershell

- set active windows border color
    https://windowsreport.com/windows-11-change-window-border/


### Make sym links from windows-os to this repo which is inside WSL

- 1. Open `Command Prompt` as admin
- 2. Find the path of your WSL folder, using powershell first
        Example: `\\wsl$\Ubuntu\home\shiva\project`
        Note: If you try to cd into this path in command prompt it wont work
- 3. use the `mklink` command
    `\\wsl$\DistroName\path\to\folder`: The path to your WSL folder.
    Example: `mklink /D "C:\path\to\link" "\\wsl$\DistroName\path\to\folder"`
    `C:\path\to\link`: The location where you want the symbolic link to appear in Windows.
    `/D` means that this is a directory link, for a file dont write `/D`

Exmaple:
Map the `chrome` folder from WSL inside the Profile folder of firefox
Source:
C:\Users\sreddy\AppData\Roaming\Mozilla\Firefox\Profiles\t2bgxzq2.default-release\
Target:
\\wsl.localhost\Ubuntu\home\shiva\dotfiles\common\firefox\chrome
Final Command:
mklink /D "C:\Users\sreddy\AppData\Roaming\Mozilla\Firefox\Profiles\t2bgxzq2.default-release\chrome" "\\wsl.localhost\Ubuntu\home\shiva\dotfiles\common\firefox\chrome"


## How to install and setup cl, clang, gcc on windows

1. GCC
install from https://www.msys2.org/
- once you install it will open a terminal like thing, inside that is where 
you need to install gcc and cc.

2. on the https://www.msys2.org/ itself it will show how to install gcc
and for cc, just do a google search 'msys2 cc' and it will show a link like this
https://packages.msys2.org/packages/mingw-w64-x86_64-cc
where it shows the command `pacman -S mingw-w64-x86_64-cc`

Successful installing of gcc and cc can be checked like this
```
shiva@tars MINGW64 ~
$ gcc --version
gcc.exe (Rev2, Built by MSYS2 project) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

shiva@tars MINGW64 ~
$ cc --version
cc.exe (Rev2, Built by MSYS2 project) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```
3. now both gcc and cc are installed. but these executables will be inside the
`c:/msys64/mingw64/bin/` so add this folder to the 'Path' in 
your system variables on windows

4. then open a new powershell or cmd, and check if you have 'gcc' and 'cc'


