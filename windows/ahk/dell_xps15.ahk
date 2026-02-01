#Requires AutoHotkey v2.0

; =============================================================================
; Swap Ctrl and Alt
; =============================================================================
LCtrl::LAlt
LAlt::LCtrl
RAlt::LAlt

; =============================================================================
; Capslock: tap for Esc, hold for nav layer
; =============================================================================
*CapsLock:: {
    KeyWait("CapsLock")
    if (A_PriorKey = "CapsLock")  ; No other key was pressed while held
        Send("{Esc}")
}

; Nav layer (Capslock + hjkl)
#HotIf GetKeyState("CapsLock", "P")
    h::Left
    j::Down
    k::Up
    l::Right
#HotIf
