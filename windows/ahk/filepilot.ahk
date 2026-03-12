#Requires AutoHotkey v2.0
#NoTrayIcon

; Win+E opens FilePilot instead of Windows Explorer
#e::Run "C:\Users\smpl\AppData\Local\Voidstar\FilePilot\FPilot.exe"

; Shift+Win+E opens Windows Explorer
+#e::Run "explorer.exe"
