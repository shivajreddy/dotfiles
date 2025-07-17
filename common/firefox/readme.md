## How to customzie firefox

Guide: https://www.userchrome.org/how-create-userchrome-css.html

# Set Up Chrome folder for customization
1. open `about:config` in firefox, and accept the risks and uncheck so it doesnt repeat
2. MUST DO THIS: Search for `toolkit.legacyUserProfileCustomizations.stylesheets` and set value to `true`
3. Get the path of the profile folder
    - Click the ` ` > `Help` > `More Troubleshooting Information` > `Profile Folder`
    - Copy to clipboard the path that is show next to the 'Open Folder' button
3. Create SymbolicLink to the Chrome folder from the Profile folder
    - Windows
    For Ubuntu with username 'smpl':
    """
    New-Item -ItemType SymbolicLink -D -Path "<copied-path-from-above>\chrome" -Target "\\wsl`$\Ubuntu\home\shiva\dotfiles\common\firefox\chrome"
    """
    Example:
    """
    New-Item -ItemType SymbolicLink -D -Path "C:\Users\shiva\AppData\Roaming\zen\Profiles\bwmqgt7s.Default (release)\chrome\userChrome.css" -Target "\\wsl$\Ubuntu\home\smpl\dotfiles\common\zenbrowser\userChrome.css"
    """
    - MacOS & Linux
    ln -s ~/dotfiles/common/firefox/chrome ~/.

4. For debugging firefox just like a webpage
    - In the Firefox Developer Tools settings (found under the three-dot menu in the upper-right corner of the DevTools pane), 
      enable "Enable browser chrome and add-on debugging toolboxes" and "Enable remote debugging
    - press ctrl+alt+shift+i to open the brower toolbox
    (or)
    - go to firefox settings(three dots) > More Tools > Browser ToolBox 

5. now go to 'Style Editor' tab, top left search for userChrome.css file, and in the editor you can update the css file and see live changes.


