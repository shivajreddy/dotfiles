#!/bin/bash

# Function to get window ID
get_window_id() {
    xdotool search --classname "floatingkitty" 2>/dev/null | head -n 1
}

# Get initial window ID
window_id=$(get_window_id)

if [ -z "$window_id" ]; then
    # No window found, launch kitty
    kitty --config ~/dotfiles/linux/kitty/floating-kitty/kitty.conf --class floatingkitty &

    # Wait for the window to appear (with timeout)
    for i in {1..20}; do
        window_id=$(get_window_id)
        if [ -n "$window_id" ]; then
            break
        fi
        sleep 0.1
    done

    # Ensure the window is floating
    i3-msg "[instance=\"floatingkitty\"] floating enable" >/dev/null
else
    # Get the focused window ID
    focused_window=$(xdotool getwindowfocus)

    if [ "$window_id" = "$focused_window" ]; then
        # If focused, kill the window
        kill $(xdotool getwindowpid "$window_id")
    else
        # If not focused, focus it
        xdotool windowactivate "$window_id"
    fi
fi
