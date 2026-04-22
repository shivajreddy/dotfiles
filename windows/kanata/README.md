# Kanata Setup

## Install

### Option 1: Winget (recommended)
```
winget install jtroo.kanata
```
This installs kanata and adds it to your system PATH.

### Option 2: Manual download
1. Download from [Kanata releases](https://github.com/jtroo/kanata/releases)
2. Choose the right variant:
   - **x64** for Intel/AMD, **arm64** for ARM
   - **gui** runs as system tray app, **tty** runs in terminal
   - **winIOv2** uses Windows hooks (recommended)
   - **wintercept** uses Interception driver (lower-level, but can disable input until reboot)
3. Extract and add to a folder on your PATH

## Config
Place `kanata.kbd` in your dotfiles kanata folder. The task is configured to use `--cfg .\kanata.kbd` relative to the exe location, so ensure `kanata.kbd` is in your working directory or pass an absolute path.

Or run manually with a custom config path:
```
kanata --cfg <path_to_config>
```

## Autostart
Use Task Scheduler to run kanata at boot with elevated privileges. Requires kanata to be installed and on PATH.

### Import the included task
1. Open Task Scheduler (`taskschd.msc`)
2. Action → Import Task → select `kanata-win-task.xml`
3. No path changes needed — the task uses `kanata` from PATH

### Or create manually
1. Open Task Scheduler → Create Task
2. General tab: Name it, check "Run with highest privileges"
3. Triggers tab: New → Begin the task "At startup"
4. Actions tab: New → Start a program
   - Program: `kanata`
   - Arguments: `--cfg <path_to_kanata.kbd>`
5. Settings tab: Check "Allow task to be run on demand"

## Stop
Task Manager → find `kanata` → End task
