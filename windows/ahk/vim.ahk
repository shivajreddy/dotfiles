; uncomment below to hide the tray icon
; #NoTrayIcon

CapsLock::
    Send {Esc}
    return


CapsLock & Enter::
    SetCapsLockState, % (GetKeyState("CapsLock", "T") ? "Off" : "On")
    return

CapsLock & h::Send {Left}
CapsLock & j::Send {Down}
CapsLock & k::Send {Up}
CapsLock & l::Send {Right}

