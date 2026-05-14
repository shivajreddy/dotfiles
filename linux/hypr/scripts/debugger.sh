#!/bin/bash

echo ""
echo "=== Debugging Special Workspace Detection ==="

echo "Full monitors output:"
hyprctl monitors

echo "Checking for special workspace:"
# Extract just the numeric ID of the special workspace
SPECIAL_ID=$(hyprctl monitors | grep -oP 'special workspace: \K-?\d+' | head -1)
echo "SPECIAL_ID value: '$SPECIAL_ID'"

# Extract the workspace name from parentheses if it exists
SPECIAL_NAME=$(hyprctl monitors | grep -oP 'special workspace: -?\d+ \(\K[^)]+' | head -1)
echo "SPECIAL_NAME value: '$SPECIAL_NAME'"

if [ -n "$SPECIAL_ID" ] && [ "$SPECIAL_ID" != "0" ]; then
    echo "RESULT: Special workspace IS active"
    echo "Special workspace detected: ID=$SPECIAL_ID, Name=$SPECIAL_NAME"
else
    echo "RESULT: No special workspace is active"
fi
echo ""
