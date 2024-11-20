## How to customzie firefox

# Set Up Chrome folder for customization
1. open `about:config` in firefox, and accept the risks and uncheck so it doesnt repeat
2. Search for `toolkit.legacyUserProfileCustomizations.stylesheets` and set value to `true`
3. Get the path of the profile folder
    - Click the ` ` > `Help` > `More Troubleshooting Information` > `Profile Folder : Open Folder`
3. Create SymbolicLink
    - Windows
    ```
    New-Item -ItemType SymbolicLink -Path "C:\Users\sreddy\.config\wezterm" -Target "\\wsl`$\Debian\home\shiva\dotfiles\common\wezterm\"
    ```
    - MacOS & Linux
    ```
    ln -s ~/dotfiles/common/firefox/chrome ~/.
    ```
