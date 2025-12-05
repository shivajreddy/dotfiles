#!/bin/sh

if pgrep -f "alacritty --class floatingterminal" > /dev/null; then
    hyprctl dispatch togglespecialworkspace floatingterminal
else
    alacritty --class floatingterminal &
    # sleep 0.5
    # hyprctl dispatch togglespecialworkspace floatingterminal
fi
