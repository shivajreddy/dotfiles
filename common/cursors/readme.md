
# HOW TO USE CUSTOM CURSORS ON LINUX

- In this example, i am using 'Bibata-BlackWhite'


1. What is the name of the cursor that linux recognizes? 
- It is the name that you given for the files cursor.theme & index.theme

2. Copy the Cursor folder, in this case it is the folder 'Biabata-BlackWhite' that has cursor.theme & index.theme files
3. Paste it into /usr/share/icons

4. Verify
So once you copy, verify by running the following and should show same output  as below

┌~
└λ ls /usr/share/icons/Bibata-BlackWhite
drwxr-xr-x    - root 29 Dec 19:52  /usr/share/icons/Bibata-BlackWhite
.rw-r--r--   77 root 29 Dec 19:52 ├──  cursor.theme
.rw-r--r--   90 root 29 Dec 19:52 ├──  cursor.theme~
drwxr-xr-x    - root 29 Dec 19:52 ├──  cursors
.rw-r--r--   79 root 29 Dec 19:52 ├──  index.theme
.rw-r--r-- 1.1k root 29 Dec 19:52 ├──  LICENSE
.rw-r--r-- 1.3k root 29 Dec 19:52 ├──  README.txt
.rw-r--r--  120 root 29 Dec 19:52 └──  VERSION


5. set the theme by editing the this file
'/usr/share/icons/default/index.theme'
Example: sudo vim /usr/share/icons/default/index.theme

And the file should look like this

```txt
[Icon Theme]
Inherits=Bibata-BlackWhite
```

6. Reboot the system

