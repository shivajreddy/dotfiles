#NoTrayIcon

; Arc browser settings
/*
F8::
    HandlePiPWindow(-112, 23) ; Minimize button (X offset = -112, Y offset = +23)
return

F9::
    HandlePiPWindow(-44, 23) ; Close button (X offset = -44, Y offset = +23)
return
*/

; Chrome browser
F8::
    HandlePiPWindow(-34, 23) ; Close button (X offset = -44, Y offset = +23)
return

; Function to handle PiP window and click at the specified offset
HandlePiPWindow(XOffset, YOffset)
{
    CoordMode, Mouse, Screen  ; Use screen coordinates for the mouse
    ; Get all open windows
    WinGet, id, list
    Loop, %id%
    {
        this_id := id%A_Index%
        WinGetTitle, this_title, ahk_id %this_id%

        ; Check if the window title contains "picture in picture" or "Picture in Picture" (case-insensitive)
        if (InStr(this_title, "picture in picture", true) or InStr(this_title, "Picture in picture", true)or InStr(this_title, "Picture-in-Picture", true) )
        {
            ; Get the position and size of the PiP window
            WinGetPos, X, Y, Width, Height, ahk_id %this_id%

            topRightX := X + Width
            topRightY := Y

            ; Calculate button coordinates based on offsets
            buttonX := topRightX + XOffset
            buttonY := topRightY + YOffset

            ; Move the mouse to the specified button and click
            DllCall("SetCursorPos", int, buttonX, int, buttonY)
            Click
            return
        }
    }
    MsgBox, No PiP window found!
}



/*
F2::
    WinGet, id, list ; Get a list of all windows
    Loop, %id%  ; Loop through all windows
    {
        this_id := id%A_Index% ; Get the window ID
        WinGetTitle, this_title, ahk_id %this_id% ; Get the title of the window

        if InStr(this_title, "picture in picture", true)
        {
            ; Get the window's position and size
            WinGetPos, X, Y, Width, Height, ahk_id %this_id%

            ; Calculate the corners
            topLeftX := X
            topLeftY := Y
            topRightX := X + Width
            topRightY := Y
            bottomLeftX := X
            bottomLeftY := Y + Height
            bottomRightX := X + Width
            bottomRightY := Y + Height

            ; Display the title and the four corner locations
            MsgBox, % "Window " A_Index ": " this_title "`n"
                . "Top-Left: (" topLeftX ", " topLeftY ")" "`n"
                . "Top-Right: (" topRightX ", " topRightY ")" "`n"
                . "Bottom-Left: (" bottomLeftX ", " bottomLeftY ")" "`n"
                . "Bottom-Right: (" bottomRightX ", " bottomRightY ")"
        }
    }
return
*/

