#!/bin/sh

# Script: toggle-special.sh
# Purpose: Toggle a Hyprland special workspace on/off with optional program launching
# Usage: toggle-special.sh [WORKSPACE_NAME] [PROGRAM] [CLASS]
# Example: toggle-special.sh floatingterminal kitty org.gatekeeper.Kitty

GIVEN_SPECIAL_WORKSPACE="${1}"

# Extract the name of the currently active special workspace from hyprctl monitors output
# Example: "special workspace: -97 (special:media)" → extract "media"
# Example: "special workspace: 0 ()" → extract empty string (no special workspace active)
ACTIVE_SPECIAL=$(hyprctl monitors | grep -oP 'special workspace: -?\d+ \(special:\K[^)]*' | head -1)

# # Debug: print variable values to log file
# {
#     echo "GIVEN_SPECIAL_WORKSPACE='$GIVEN_SPECIAL_WORKSPACE'"
#     echo "ACTIVE_SPECIAL='$ACTIVE_SPECIAL'"
# } >> ~/.config/hypr/scripts/toggle-special.log

# Determine whether a special workspace is currently active
if [ -n "$ACTIVE_SPECIAL" ]; then
    # A special workspace IS currently active (ACTIVE_SPECIAL contains a workspace name)
    # Action: Toggle OFF the currently active special workspace
    hyprctl dispatch togglespecialworkspace "$ACTIVE_SPECIAL"


else
    # NO special workspace is currently active (ACTIVE_SPECIAL is empty)
    # Action: Toggle ON the special workspace passed as the first argument

    # Toggle the special workspace on
    hyprctl dispatch togglespecialworkspace "$GIVEN_SPECIAL_WORKSPACE"

    # After turning on special workspace, do the dedicated logic for special workspaces
    # such as bring up windows if they are not already present

    # Special handling for floatingterminal: ensure alacritty is running
    if [ "$GIVEN_SPECIAL_WORKSPACE" = "floatingterminal" ]; then
        # If no alacritty terminal with class 'floatingterminal' is running, spawn one
        # This ensures the floating terminal window exists before toggling the workspace visible
        if ! pgrep -f "alacritty.*--class floatingterminal" > /dev/null 2>&1; then
            hyprctl dispatch exec "alacritty --class floatingterminal"
        fi
    fi

    # make sure spotify is always there
    # if [ "$GIVEN_SPECIAL_WORKSPACE" = "media" ]; then
    #     if ! pgrep -f "spotify" > /dev/null 2>&1; then
    #         hyprctl dispatch exec "spotify"
    #     fi
    # fi

fi
