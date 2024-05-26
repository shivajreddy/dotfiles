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

path="$1"
user_home="/home/shiva"

# Replace $user_home with ~ in the path
path="${path/#$user_home/\~}"

fixed_length=$2

if (( ${#path} > $fixed_length )); then
    path=$(echo "$path" | tail -c "$fixed_length")
    if ! [[ "$path" =~ ^//\* ]]; then
        path="/${path#\*/}"
    fi
    path="...$path"
fi

echo $path
