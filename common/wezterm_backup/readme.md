# Set up dotfiles for Wezterm


- How to set up config is at ::
  https://wezfurlong.org/wezterm/config/files.html#quick-start

## MacOs & Linux

```
ln -s ~/dotfiles/common/wezterm ~/.config/wezterm
```

## Windows

    - Best way is to create a '.config' folder under the home folder  
    so 'cd ~' to go to the home folder, and create a new '.config' folder if doesn't exist  
    Then create a symlink to the wezterm dots  

    - This is how you create a symlink
    - Command Prompt:
    ```
    mklink /D "C:\Users\sreddy\.config\wezterm" "\\wsl$\Ubuntu\home\shiva\dotfiles\common\wezterm"
    ```
    - PowerShell-7:
    ```
    New-Item -ItemType SymbolicLink -Path "C:\LinkFolder\MyLink.txt" -Target "C:\OriginalFolder\OriginalFile.txt"
    ```
    - Actual commands i used
    ```
    New-Item -ItemType SymbolicLink -Path "C:\Users\sreddy\.config\wezterm" -Target "\\wsl`$\Debian\home\shiva\dotfiles\common\wezterm\"
    New-Item -ItemType SymbolicLink -Path "C:\Users\sreddy\.config\wezterm" -Target "\\wsl`$\Ubuntu\home\shiva\dotfiles\common\wezterm\"

    mklink /D "C:\Users\sreddy\.config\wezterm" "\\wsl$\Debian\home\shiva\dotfiles\common\wezterm"
    ```
    NOTE:  
    After creating a symlink wezterm might open and crash, use windows terminal to edit the dotfiles in that case  
    Most common situation: the dots have default domain of wezterm set to wsl distro debian, if you installed Ubuntu  
    then comment the debian settings block and uncomment ubuntu settings block under 'wezterm.lua'


# Workspaces

- This dude's config finally showed how to use the plugin
    - https://fredrikaverpil.github.io/blog/2024/10/20/session-management-in-wezterm-without-tmux/
    - https://github.com/fredrikaverpil/dotfiles/blob/main/wezterm.lua

    - I have workspace switcher loaded and set up under win_keys.lua
   - TODO: I have to move this main wezterm.lua so that, it works on mac too
