#!/bin/bash

# Get the current mode
current_mode=$(i3-msg -t get_binding_state)

if [[ "$current_mode" == "paused" ]]; then
    # Switch back to default mode
    i3-msg mode "default"
else
    # Switch to paused mode
    i3-msg mode "paused"
fi
