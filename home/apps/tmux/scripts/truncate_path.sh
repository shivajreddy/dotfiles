#!/bin/sh
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
