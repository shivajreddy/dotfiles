
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

