# Step 5: Install Neovim and Set Up LazyVim Starter

echo "🌱 Step 5: Installing Neovim and Setting Up LazyVim Starter..."



# Function to install Neovim if not already installed

install_neovim() {

    if ! command -v nvim &> /dev/null; then

        echo "🔧 Adding Neovim PPA..."

        sudo add-apt-repository -y ppa:neovim-ppa/stable >/dev/null 2>&1

        echo "⏳ Updating package list..."

        sudo apt update -qq

        echo "⏳ Installing Neovim..."

        sudo apt install -y -qq neovim && echo "✅ Neovim installed successfully."

    else

        echo "🌊 Neovim is already installed."

    fi

}



install_neovim



# Clear Neovim-related cache, data, and state

echo "🚮 Removing old Neovim cache, data, and state..."

rm -rf ~/.local/share/nvim{,.bak} && echo "✅ Removed old Neovim data."

rm -rf ~/.local/state/nvim{,.bak} && echo "✅ Removed old Neovim state."

rm -rf ~/.cache/nvim{,.bak} && echo "✅ Removed old Neovim cache."



echo "🎉 Neovim setup cleanup complete! Ready to set up LazyVim."


