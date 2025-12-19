#!/bin/bash

# Monitor hyprland submap and output pause status
socat -U - UNIX-CONNECT:/run/user/1000/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket.sock | while read -r line; do
    if [[ "$line" == "submap>>"* ]]; then
        submap="${line#submap>>}"
        if [[ "$submap" == "pause" ]]; then
            echo '{"text":"󰐊 paused","class":"paused"}'
        else
            echo '{"text":"","class":"active"}'
        fi
    fi
done
