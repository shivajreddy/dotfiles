#!/bin/bash

# Get the current folder name as the project name
PROJECT_NAME=$(basename "$PWD")

# Print the project name
echo "Project name is: $PROJECT_NAME"

# Create and configure the build directory
echo "Configuring project $PROJECT_NAME..."
cmake -B build/ -DPROJECT_NAME="$PROJECT_NAME"

# Build the project
echo "Building project $PROJECT_NAME..."
cmake --build build/

# Monitor the file for changes
echo "Watching CPP/H files for changes. Press Ctrl+C to stop."
find . -type f \( -name "*.cpp" -o -name "*.h" \) | entr -c sh -c "cmake --build build/ && clear && ./build/$PROJECT_NAME"
