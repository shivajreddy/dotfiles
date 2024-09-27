#!/bin/bash

function todo() {
  local current_dir=$(pwd)
  cd ~/todo
  nvim todo.md
  cd "$current_dir"
}
