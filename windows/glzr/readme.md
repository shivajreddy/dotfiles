# Glaze Tiling Manager

- glaze looks for `.glzr` file in Home directory of windows
  - Home is `C:/Users/shiva`
- Use the following command in powershell to map this entire directory

  ```bash
  New-Item -ItemType SymbolicLink -Path "C:\Users\shiva\.glzr" -Target "\\wsl$\Debian\home\shiva\dotfiles\windows\glzr"
  ```
