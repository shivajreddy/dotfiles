## How to customzie firefox

# Set Up Chrome folder for customization
1. open `about:config` in firefox, and accept the risks and uncheck so it doesnt repeat
2. Search for `toolkit.legacyUserProfileCustomizations.stylesheets` and set value to `true`
3. Get the path of the profile folder
    - Click the ` ` > `Help` > `More Troubleshooting Information` > `Profile Folder`
    - Copy to clipboard the path that is show next to the 'Open Folder' button
3. Create SymbolicLink to the Chrome folder from the Profile folder
    - Windows
    ```
    For Debian with username 'shiva':
    New-Item -ItemType SymbolicLink -D -Path "<copied-path-from-above>\chrome" -Target "\\wsl`$\Debian\home\shiva\dotfiles\common\firefox\chrome"
    For Ubuntu with username 'shiva':
    New-Item -ItemType SymbolicLink -D -Path "<copied-path-from-above>\chrome" -Target "\\wsl`$\Ubuntu\home\shiva\dotfiles\common\firefox\chrome"
    Example:
    New-Item -ItemType SymbolicLink -D -Path "C:\Users\sreddy\AppData\Roaming\Mozilla\Firefox\Profiles\8vanq41s.default-release\chrome" -Target "\\wsl`$\Debian\home\shiva\dotfiles\common\firefox\chrome"
    ```
    - MacOS & Linux
    ```
    ln -s ~/dotfiles/common/firefox/chrome ~/.
    ```
