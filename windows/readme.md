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


# Windows C, C++ Compilers setup

To install the GNU Compiler Collection (GCC) on Windows 11, you can use the MSYS2 environment, which provides a Unix-like shell and a package management system for Windows. Here's a step-by-step guide:

Install MSYS2:

1.  Download the MSYS2 installer from the [official website](https://www.msys2.org/)
- You can install at **C** or other if prefered

2. Update MSYS2:
- Open the MSYS2 Shell that was installed with the MSYS2 package.
- Run the following commands to update the package database and core system packages:
```
pacman -S mingw-w64-x86_64-toolchain
```
- choose all,  This will install the GCC compiler and related tools.
- This may take some time as it updates the system.

3. Add GCC to the System Path:
- Once the installation is complete, you need to add the GCC binaries to the system path so that you can run them from any command prompt.
- Locate the installation directory (default is `C:\msys64`).
	- Make sure to adjust the path if you installed MSYS2 in a different directory.
- Add the bin directory to the system path. You can do this by adding the following line to your system or user environment variables: `C:\msys64\mingw64\bin`

4. Verify Installation:
- Open a new powershell and type the following `gcc` command to verify that GCC is inst
- `gcc --version`
