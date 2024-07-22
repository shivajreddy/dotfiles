#!/bin/bash

todo() {
  local current_dir=$(pwd)
  cd ~/todo
  vi todo.md
  cd "$current_dir"
}

todo
