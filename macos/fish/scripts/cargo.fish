# Shortcuts for faster cargo stuff

function cwi
    cargo watch -x "install --path ."
end

function cwr
    cargo watch -q -c -x "run -q"
end

function cwe
    cargo watch -q -c -x "run -q --example '$argv[1]'"
end

# - `cwt lifetimes` is equal to `cargo test --test lifetimes -- --nocapture`
# -     meaning it will run the file at 'project_root/tests/lifetimes.rs'
# - `cwt test_file_name test_my_fn` run a test inside a target file

function cwt
    if test (count $argv) -eq 1
        cargo watch -q -c -x "test --test '$argv[1]' -- --nocapture"
    else if test (count $argv) -eq 2
        cargo watch -q -c -x "test --test '$argv[1]' '$argv[2]' -- --nocapture"
    else
        cargo watch -q -c -x "test -- --nocapture"
    end
end
