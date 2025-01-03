- Remove existing config

```bash
rm -rf ~/.local/share/chatterino/Settings
rm -rf ~/.local/share/chatterino/Themes
```

Sym link this to where chatterino looks for config

```bash
ln -s ~/dotfiles/linux/chatterino/Settings ~/.local/share/chatterino/Settings
ln -s ~/dotfiles/linux/chatterino/Themes ~/.local/share/chatterino/Themes
```
