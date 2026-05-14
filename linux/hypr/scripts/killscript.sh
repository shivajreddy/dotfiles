#!/bin/sh
window_class=$(hyprctl activewindow | sed -n 's/^.*class: //p')

# echo "Window class: '$window_class'" >> "$(dirname "$0")/debug.log"

if [ "$window_class" = "discord" ]; then
    hyprctl dispatch forcekillactive
    killall discord
elif [ "$window_class" = "Emacs" ]; then
    # echo "Blocked Emacs" >> "$(dirname "$0")/debug.log"
    hyprctl dispatch killactive
else
    # https://wiki.hypr.land/Configuring/Dispatchers/#list-of-dispatchers
    # killactive - closes (not kills) the active window
    # forcekillactive - kills the active window
    hyprctl dispatch killactive
    # hyprctl dispatch forcekillactive
fi

