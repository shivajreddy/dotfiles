
{ config, pkgs, inputs, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    ];


  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot.loader.grub.enable = true;
  # boot.loader.grub.device = "/dev/????";
  # boot.loader.grub.useOSProber = true;

  networking.hostName = "tars"; # Define your hostname.

  networking.networkmanager.enable = true;
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable Bluetooth
  hardware.bluetooth.enable = true;

  # ------ DESKTOP ENVIRONMENT ------
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment..
  services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;

  # services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.displayManager.lightdm.greeters.gtk.enable = true;

  # Configure keymap in X11
  services.xserver = {
  xkb.layout = "us";
  xkb.variant = "";
  };

  # --- HYPRLAND ----
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  programs.zsh.enable = true;

  environment.sessionVariables = {
    # if cursor is not workign then set the below to 1
    # WLR_NO_HARDWARE_CURSORS = "1";
    
    # Hint electron apps to use wayland
    NIXOS_OZONE_WL = "1";
  };

  hardware = {
    # opengl
    opengl.enable = true;
    
    # Most wayland compositors need this
    # nvidia.modesetting.enable = true;
  };

  # ###### OTHER STUFF ######
  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Define a user account.
  users.users.shiva = {
    isNormalUser = true;
    description = "shiva";
    extraGroups = [ "networkmanager" "wheel" ];

    shell = pkgs.zsh;

    packages = with pkgs; [
      firefox
      brave
      kitty
      neovim
      jetbrains-mono
      rofi-wayland
      bluez
      blueman
      pavucontrol
      htop
      btop
      swww
      xfce.thunar
      gvfs  # for thumbdrive to work with thunar
      xfce.thunar-volman
      xfce.tumbler
      nomacs
      spotify
      obs-studio
      starship
      neofetch
      wev	# wayland event viewer, for keystrokes
      playerctl
      swayosd
      wl-clipboard
      zsh-autosuggestions
      eza
    ];
  };


  # Allow unfree packages.
  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.permittedInsecurePackages = [
    "python-2.7.18.7"
  ];

  fonts.packages = with pkgs; [
    (nerdfonts.override{ fonts = ["JetBrainsMono"];})
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    unzip
    vim 
    kitty
    zsh
    waybar
    dunst
    libnotify
    vscode
    python3
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];


  system.stateVersion = "23.11"; # DON'T CHANGE THIS

}
