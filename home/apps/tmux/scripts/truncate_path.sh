#!/bin/sh

: '
path="$1"
fixed_length=$2
if (( ${#path} > $fixed_length )); then
    path=$(echo "$path" | tail -c "$fixed_length")
    if ! [[ "$path" =~ ^//* ]]; then
        path="/${path#*/}"
    fi
    path="...$path"
fi
echo $path
'


# Variables
path="$1"
fixed_length=$2
username="shiva"
home_dir="/home/$username"

# Replace /home/<user> with ~
path="${path/#$home_dir/~}"

# Check if path length exceeds the fixed length
if [ ${#path} -gt $fixed_length ]; then
    path=$(echo "$path" | tail -c "$fixed_length")
    if ! [[ "$path" =~ ^//* ]]; then
        path="/${path#*/}"
    fi
    path="...$path"
fi

# Output the result
echo $path
