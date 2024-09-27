
1. go to chrome://extensions/
2. enable developer mode
3. load "rose-pine" or "rose-pine-moon" folder as unpacked extension
4. open devtools, go to settings->experiments and check "Allow extensions to load custom stylesheets"

## Note for catpppuccin:
- repo link: https://github.com/catppuccin/chrome-devtools
- the directions on the repo wont work as you expect because of some broken sys links and reference paths
- i made the 'catppuccin-mocha' using rose-pine as a guide with all the css from the catppuccin repo,
  so if you want to change the accent or to other catppuccin flavor, just go to /themes/<flavor_folder/ in the repo link  
  and copy all the contents of 'devtools.css' and paste them in your 'catppuccin-mocha/mocha.css' file  
  and then reload the extension in chrome extensions
