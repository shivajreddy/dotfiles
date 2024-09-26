#!/bin/bash

function cwi() {
  cargo watch -x "install --path ."
}

function cwr() {
  cargo watch -q -c -x "run -q"
}

function cwe() {
  cargo watch -q -c -x "run -q --example '$1'"
}

# - `cwt lifetimes` is equal to `cargo test --test lifetimes -- --nocapture`
# -     meaning it will run the file at 'project_root/tests/lifetimes.rs'
# - `cwt test_file_name test_my_fn` run a test inside a target file

function cwt() {
  if [[ $# -eq 1 ]]; then
    # cargo watch -q -c -x "test '$1' -- --nocapture"
    cargo watch -q -c -x "test --test '$1' -- --nocapture"
  elif [[ $# -eq 2 ]]; then
    cargo watch -q -c -x "test --test '$1' '$2' -- --nocapture"
  else
    cargo watch -q -c -x "test -- --nocapture"
  fi
}
