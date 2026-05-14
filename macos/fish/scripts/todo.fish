
function todo
    set current_dir (pwd)
    cd ~/todo
    nvim todo.md
    cd $current_dir
end
