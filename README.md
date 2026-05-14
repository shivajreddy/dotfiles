# dotfiles

Cross-platform configuration files for Windows, macOS, and Linux.

> `*` = actively used

## Structure

```
dotfiles/
├── common/          # Shared across all platforms
├── linux/           # Arch/Hyprland specific
├── macos/           # macOS specific
└── windows/         # Windows specific
```

## Quick Setup

```bash
# Clone
git clone https://github.com/YOUR_USERNAME/dotfiles.git ~/.dotfiles

# Symlink what you need (example)
ln -s ~/.dotfiles/common/nvim ~/.config/nvim
ln -s ~/.dotfiles/common/wezterm ~/.config/wezterm
```

---

## Common (Cross-Platform)

| Tool | Config | Description |
|------|--------|-------------|
| [Neovim](https://neovim.io/) * | [`common/nvim`](common/nvim) | LazyVim-based config with LSP, smart-splits, yazi |
| [Doom Emacs](https://github.com/doomemacs/doomemacs) * | [`common/doom`](common/doom) | Emacs config with evil-mode |
| [WezTerm](https://wezfurlong.org/wezterm/) * | [`common/wezterm`](common/wezterm) | Terminal with workspaces, smart-splits integration |
| [Starship](https://starship.rs/) * | [`common/starship.toml`](common/starship.toml) | Minimal prompt with git, language info |
| [Yazi](https://yazi-rs.github.io/) * | [`common/yazi`](common/yazi) | Terminal file manager |
| [btop](https://github.com/aristocratos/btop) * | [`common/btop`](common/btop) | Resource monitor |

---

## Linux

**Window Manager:** [Hyprland](https://hyprland.org/)

| Tool | Config | Description |
|------|--------|-------------|
| [Hyprland](https://hyprland.org/) * | [`linux/hypr`](linux/hypr) | Wayland compositor (keybinds, workspaces, decorations) |
| [Waybar](https://github.com/Alexays/Waybar) | [`linux/waybar`](linux/waybar) | Status bar |
| [Rofi](https://github.com/davatorium/rofi) | [`linux/rofi`](linux/rofi) | App launcher |
| [Dunst](https://dunst-project.org/) | [`linux/dunst`](linux/dunst) | Notifications |
| [Kitty](https://sw.kovidgoyal.net/kitty/) | [`linux/kitty`](linux/kitty) | Terminal |
| [Alacritty](https://alacritty.org/) | [`linux/alacritty`](linux/alacritty) | GPU terminal |
| [Picom](https://github.com/yshui/picom) | [`linux/picom`](linux/picom) | Compositor (X11) |
| [Fish](https://fishshell.com/) | [`linux/fish`](linux/fish) | Shell |
| [tmux](https://github.com/tmux/tmux) | [`linux/tmux`](linux/tmux) | Terminal multiplexer |
| [i3](https://i3wm.org/) | [`linux/i3`](linux/i3) | X11 window manager |

---

## macOS

| Tool | Config | Description |
|------|--------|-------------|
| [AeroSpace](https://github.com/nikitabobko/AeroSpace) | [`macos/aerospace`](macos/aerospace) | i3-like tiling WM |
| [Karabiner](https://karabiner-elements.pqrs.org/) | [`macos/karabiner`](macos/karabiner) | Keyboard remapping |
| [Kanata](https://github.com/jtroo/kanata) | [`macos/kanata`](macos/kanata) | Advanced key remapping |
| [skhd](https://github.com/koekeishiya/skhd) | [`macos/skhd`](macos/skhd) | Hotkey daemon |
| [yabai](https://github.com/koekeishiya/yabai) * | [`macos/yabai`](macos/yabai) | Tiling WM |
| [Kitty](https://sw.kovidgoyal.net/kitty/) | [`macos/kitty`](macos/kitty) | Terminal |
| [Alacritty](https://alacritty.org/) | [`macos/alacritty`](macos/alacritty) | GPU terminal |
| [tmux](https://github.com/tmux/tmux) | [`macos/tmux`](macos/tmux) | Terminal multiplexer |
| [Fish](https://fishshell.com/) | [`macos/fish`](macos/fish) | Shell |
| [Raycast](https://www.raycast.com/) | [`macos/raycast`](macos/raycast) | Launcher |
| [gh](https://cli.github.com/) | [`macos/gh`](macos/gh) | GitHub CLI |

---

## Windows

| Tool | Config | Description |
|------|--------|-------------|
| [PowerShell](https://github.com/PowerShell/PowerShell) * | [`windows/PowerShell`](windows/PowerShell) | Shell profile |
| [WezTerm](https://wezfurlong.org/wezterm/) * | [`common/wezterm`](common/wezterm) | Terminal (uses common config) |
| [Windows Terminal](https://github.com/microsoft/terminal) | [`windows/terminal`](windows/terminal) | Terminal settings |
| [Alacritty](https://alacritty.org/) | [`windows/alacritty`](windows/alacritty) | GPU terminal |
| [Kanata](https://github.com/jtroo/kanata) * | [`windows/kanata`](windows/kanata) | Keyboard remapping |
| [AutoHotkey](https://www.autohotkey.com/) | [`windows/ahk`](windows/ahk) | Automation scripts |
| [tacky-borders](https://github.com/lukeyou05/tacky-borders) * | [`windows/tacky-borders`](windows/tacky-borders) | Window borders |
| [GlazeWM](https://github.com/glzr-io/glazewm) * | [`windows/glzr`](windows/glzr) | Tiling WM |
| [Visual Studio](https://visualstudio.microsoft.com/) * | [`windows/visual-studio`](windows/visual-studio) | IDE settings |
| [Neovim](https://neovim.io/) * | [`windows/nvim`](windows/nvim) | Windows-specific nvim |
| [WSL](https://learn.microsoft.com/en-us/windows/wsl/) * | [`windows/wsl`](windows/wsl) | WSL configs |

---

## Key Features

**Neovim** ([`common/nvim`](common/nvim))
- [LazyVim](https://www.lazyvim.org/) base
- LSP + Mason for language servers
- [smart-splits](https://github.com/mrjones2014/smart-splits.nvim) for seamless pane navigation with WezTerm

**WezTerm** ([`common/wezterm`](common/wezterm))
- Modular config: `ui.lua`, `keys.lua`, `utils.lua`
- [smart-splits](https://github.com/mrjones2014/smart-splits.nvim) integration (Ctrl+hjkl)
- [Workspace switcher](https://github.com/MLFlexer/smart_workspace_switcher.wezterm) with zoxide

**Hyprland** ([`linux/hypr`](linux/hypr))
- Split config: keybinds, workspaces, decorations, monitors
- Hyprlock, Hyprpaper

---

## License

MIT
