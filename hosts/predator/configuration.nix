{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
  ];


  # Bootloader
  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
    };
    grub = {
      enable = true;
      efiSupport = true;

      device = "nodev";
      # device = "nvme0n1p1";  # Ensure this is the disk where your EFI partition is located
      # lsblk
      # ├─nvme0n1p1 259:1    0   100M  0 part /boot

      # useOSProber = true;   # This allows GRUB to detect Windows.
    };
  };


  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Networking
  networking.hostName = "predator";
  networking.networkmanager.enable = true;

  # NOTE: DONT DO THIS EVER
  # networking.firewall.enable = false;

  # TimeZone
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

  # Hardware
  hardware = {
    opengl.enable = true;
    bluetooth.enable = true;
    nvidia.modesetting.enable = true; # Most wayland compositors need this
  };

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



  # #### Environment Settings ####
  # #### Apps ####
  environment.systemPackages = import ../apps/default.nix {inherit pkgs;};

  environment.shells = with pkgs; [zsh]; # Shells
  environment.sessionVariables = {
    # Session Variables
    # WLR_NO_HARDWARE_CURSORS = "1";	 # if cursor is not workign then set the below to 1
    NIXOS_OZONE_WL = "1"; # Hint electron apps to use wayland
  };

  environment.variables = {
    OPENSSL_DEV = pkgs.openssl.dev;
  };

  # User account
  users.users.shiva = {
    isNormalUser = true;
    description = "shiva";
    extraGroups = ["networkmanager" "wheel"];
    # shell = pkgs.zsh;  # moved all user packages to home-manager, will cuz error????
    packages = with pkgs; [];
  };
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  # DESKTOP ENVIRONMENT
  services.xserver.enable = true; # Enable the X11 windowing system.
  services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome.enable = true;

  services.gvfs.enable = true;

  # Configure keymap in X11
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
  };

  # HYPRLAND
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # Fonts
  fonts.packages = with pkgs; [
    ibm-plex
    (nerdfonts.override {fonts = ["JetBrainsMono" "Iosevka"];})
  ];

  # System Services
  services = {
    avahi = {
      nssmdns4 = true;
      enable = true;
      publish = {
        enable = true;
        userServices = true;
        domain = true;
      };
    };
  };

  # NIX settings
  nix.settings.experimental-features = ["nix-command" "flakes"]; # enable flakes
  system.stateVersion = "23.11"; # DON'T CHANGE THIS
}
