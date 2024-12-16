#!/bin/bash

# Check if a project name argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <project_name>"
    exit 1
fi

PROJECT_NAME=$1

# Create and configure the build directory
echo "Configuring project $PROJECT_NAME..."
cmake -B build/ -DPROJECT_NAME=$PROJECT_NAME

# Build the project
echo "Building project $PROJECT_NAME..."
cmake --build build/

# Monitor the file for changes using find and entr
echo "Watching CPP/H files for changes. Press Ctrl+C to stop."
# find . -type f \( -name "*.cpp" -o -name "*.h" \) | entr -c sh -c "./build/visualize"

find . -type f \( -name "*.cpp" -o -name "*.h" \) | entr -c sh -c "cmake --build build/ && clear && ./build/$PROJECT_NAME"
