For windows i use the following
https://github.com/aristocratos/btop4win/
which can be downloaded using `winget install aristocratos.btop4win`

https://github.com/aristocratos/btop4win/issues/67
- the themes folder should be next to the btop4win.exe BUT that doesnt work
- the workaround is to place the themes folder at
C:\Users\smpl\AppData\Local\Microsoft\WinGet\Links

so go that path and paste the following
┌C:\Users\smpl\AppData\Local\Microsoft\WinGet\Links
└  New-Item -ItemType SymbolicLink -Path ./themes -Target "C:\Users\smpl\dotfiles\common\btop\themes\"

now when you reopen btop, and go to esc > options > you should the themes from our folder

