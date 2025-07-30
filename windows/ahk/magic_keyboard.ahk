; Uncomment the line below to hide the tray icon
; #NoTrayIcon

; --- CapsLock Layer for navigation ---
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


; --- Remap Left Command (LWin on Magic Keyboard) to Left Ctrl ---
LWin::LCtrl
; --- Remap Left Ctrl to Left Win ---
LCtrl::LWin


; use the normal function keys as the designed media key
; F7::Send {Media_Prev}
; F8::Send {Media_Play_Pause}
; F9::Send {Media_Next}
F10::Send {Volume_Mute}
F11::Send {Volume_Down}
F12::Send {Volume_Up}

; use capslock to actually use the function key
; CapsLock & F7::Send {F7}
; CapsLock & F8::Send {F8}
; CapsLock & F9::Send {F9}
; CapsLock & F10::Send {F10}
; CapsLock & F11::Send {F11}
; CapsLock & F12::Send {F12}
