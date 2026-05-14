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

### EMACS On Windows

1. Make sure to install msys64
2. Just read over the file at './readme/build_emacs_windows.md'
3. Now follow that file and finish all the steps, so that now we have all the 
   dependencies ready to get emacs with native comp on windows.
4. Go to this official link, https://www.gnu.org/software/emacs/download.html#nonfree
    it shows the command to install emacs, which is `pacman -S mingw-w64-x86_64-emacs`
    So run that command in the msys64 terminal, and that will install emacs 
    in the folder "C:\msys64\mingw64\bin", if msys64 was installed properly then this bin 
    folder was already included in the system path.
5. Start emacs using 'runemacs.exe' so that it opens with out a termianl,
   if emacs.exe opens with out a terminal then great, just use that.
   but to use 'emacs' from start but not have it open with out the terminal, 
   right click on emacs from the start, and go to open file location, which will show the 
   shortcut file at "C:\Users\shiva\AppData\Roaming\Microsoft\Windows\Start Menu\Programs"
   and Right-click on the shortcut file → Properties → Change "Run" to "Minimized"
6. config for native emacs will be located at appdata/roaming/.emacs.d folder
7. Emacs needs 'NFM' font besides the Nerd Font, you download the NFM font file via emacs by running `M-x nerd-icons-install-fonts`

### Doom EMACS On Windows
1. PREREQS
- official link https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org
- we will be using the third option "With a precompiled binary + Git Bash"
    - we already have emacs installed, we have to install 'ripgrep' and 'fd' and both to PATH
    - for both 'ripgrep' and 'fd' use the msvc-x86_64 zip files, unzip them and paste them in C:/bin folder
        - fd: pacman -S mingw-w64-x86_64-fd
        - ripgrep: pacman -S mingw-w64-x86_64-ripgrep
        - make sure that 'fd' and 'rg' works in powershell
    - make sure to add that C:/bin also to path, so that 'rg' and 'fd' are added to the path, and test them on powershell

2. CONFIG
- doom will look for its config in the appdata/roaming/.doom.d folder, so 
 symlink .doom.d folder to dotfiles/common/doom by using the following command from the roaming folder
```
┌~/AppData/Roaming
└  New-Item -ItemType SymbolicLink -Path .\.doom.d -Target "C:\Users\shiva\dotfiles\common\doom\"
```

3. INSTALLING DOOM COMMANDLINE UTILITIES
- Clone the doomemacs repo into the users appdata/roaming directory with the name .emacs.d
```

└  pwd
C:\Users\shiva\AppData\Roaming

┌\C:\Users\shiva\AppData\Roaming
└  git clone https://github.com/doomemacs/doomemacs.git .emacs.d
Cloning into '.emacs.d'...
remote: Enumerating objects: 137033, done.
remote: Counting objects: 100% (237/237), done.
remote: Compressing objects: 100% (94/94), done.
remote: Total 137033 (delta 184), reused 143 (delta 143), pack-reused 136796 (from 3)
Receiving objects: 100% (137033/137033), 33.91 MiB | 10.45 MiB/s, done.
Resolving deltas: 100% (95721/95721), done.
Updating files: 100% (1103/1103), done.
```

- So basically we want the doomemacs repo to be git clone into the roaming folder with the name .emacs.d,
  and sym link dotfiles/common/doom to roaming/.doom.d

- the doom command line utilities are now with in the folder roamin/.emacs.d/bin, so add 
roaming/.emacs.d/bin to PATH
- after you do this, reopen powershell, and you should be able to type 'doom' command
- Now we have emacs(with native comp), ripgrep, fd. and all three can be opened from terminal
- a. run 'doom install' 
    - now since we already linked the .doom.d folder to dotfiles/common/doom, 
    it will suggest that skipping the .doom.d folder since there is already that folder
- b. 'doom sync' and 'doom update' should work as expected

