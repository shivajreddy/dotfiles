# Kanata Setup

## Install

### Option 1: Winget (installs GUI variant)
```
winget search kanata
```
Install the one by jtroo. This installs `kanata_windows_gui_winIOv2_x64.exe`.

### Option 2: Manual download
1. Download from [Kanata releases](https://github.com/jtroo/kanata/releases)
2. Choose the right variant:
   - **x64** for Intel/AMD, **arm64** for ARM
   - **gui** runs as system tray app, **tty** runs in terminal
   - **winIOv2** uses Windows hooks (recommended)
   - **wintercept** uses Interception driver (lower-level, but can disable input until reboot)
3. Extract and place the exe where you want it

## Config
Place `kanata.kbd` next to the exe and double-click to run.

Or use a custom config path:
```
kanata_windows_gui_winIOv2_x64.exe --cfg <path_to_config>
```

## Autostart 
Use Task Scheduler to run kanata at boot with elevated privileges.

### Import the included task
1. Open Task Scheduler (`taskschd.msc`)
2. Action → Import Task → select `run kanata.xml`
3. Adjust the paths if needed to match your setup

### Or create manually
1. Open Task Scheduler → Create Task
2. General tab: Name it, check "Run with highest privileges"
3. Triggers tab: New → Begin the task "At startup"
4. Actions tab: New → Start a program
   - Program: `<path>\kanata_windows_gui_winIOv2_x64.exe`
   - Arguments: `--cfg .\kanata.kbd`
5. Settings tab: Check "Allow task to be run on demand"

## Stop
Task Manager → find `kanata_windows_gui_winIOv2_x64.exe` → End task
