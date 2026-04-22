# Kanata Setup

## Install

### Option 1: Winget (recommended)
```
winget install jtroo.kanata
```
Winget installs the binary to:
```
%USERPROFILE%\AppData\Local\Microsoft\WinGet\Links\kanata.exe
```
The included task files point to this exact path. If you install via winget, the tasks will work as-is after import.

### Option 2: Manual download
1. Download from [Kanata releases](https://github.com/jtroo/kanata/releases)
2. Choose the right variant:
   - **x64** for Intel/AMD, **arm64** for ARM
   - **gui** runs as system tray app, **tty** runs in terminal
   - **winIOv2** uses Windows hooks (recommended)
   - **wintercept** uses Interception driver (lower-level, but can disable input until reboot)
3. Place the exe at `%USERPROFILE%\AppData\Local\Microsoft\WinGet\Links\kanata.exe` to match the task files, or update the `<Command>` path in the task file after importing

## Config
Each device has its own config file:
- `kanata-dellxps.kbd`
- `kanata-dell-pro-max-16-premium.kbd`
- `kanata-logitech-k850.kbd`

The tasks expect these files at `%USERPROFILE%\dotfiles\windows\kanata\`, i.e. this folder must be cloned to `%USERPROFILE%\dotfiles`.

Or run manually with a custom config path:
```
kanata --cfg <path_to_config>
```

## Autostart
Use Task Scheduler to run kanata at boot with elevated privileges.

**Prerequisites for the task to work after import:**
- Kanata installed via winget (binary at `%USERPROFILE%\AppData\Local\Microsoft\WinGet\Links\kanata.exe`)
- Dotfiles cloned to `%USERPROFILE%\dotfiles`

### Import the included task
1. Open Task Scheduler (`taskschd.msc`)
2. Action → Import Task → select the task file for your device:
   - `kanata-task-dellxps.xml`
   - `kanata-task-dell-pro-max-16-premium.xml`
   - `kanata-task-logitech-k850.xml`
3. No changes needed — paths use `%USERPROFILE%` and resolve to the current user automatically

### Or create manually
1. Open Task Scheduler → Create Task
2. General tab: Name it, check "Run with highest privileges"
3. Triggers tab: New → Begin the task "At startup"
4. Actions tab: New → Start a program
   - Program: `%USERPROFILE%\AppData\Local\Microsoft\WinGet\Links\kanata.exe`
   - Arguments: `--cfg %USERPROFILE%\dotfiles\windows\kanata\kanata-<device>.kbd`
5. Settings tab: Check "Allow task to be run on demand"

## Stop
Task Manager → find `kanata` → End task
