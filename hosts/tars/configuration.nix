{ config, pkgs, inputs, ... }:

{
  imports = [ 
    ./hardware-configuration.nix
    (../apps/pcloud/default.nix)
  ];
  
  /*  NIX PATH FIX
  nix.nixPath = [
    "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
    "/nix/var/nix/profiles/per-user/root/channels"
  ];
  # */ 


  # GPU modules
  boot.initrd.kernelModules = [ "amdgpu" ];

  # Bootloader.
  boot.loader = {
    # systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;

    grub.enable = true;
    grub.efiSupport = true;
    # https://nixos.org/manual/nixos/stable/options#opt-boot.loader.grub.device
    grub.device = "nodev";
    # grub.device = "/nvme0n1/nvme0n1p1";
  };


  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Networking
  networking.hostName = "tars";
  networking.networkmanager.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 ];

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
    nvidia.modesetting.enable = true; # Most wayland compositors need this
  };

  hardware.bluetooth = {
    enable = true;
    # powerOnBoot = true;
    # settings = {
    #  General = {
    #    ControllerMode = "bredr";
    #  };
    # };
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
  environment.systemPackages = import ../apps/default.nix { inherit pkgs; };

  environment.shells = with pkgs; [ zsh ];	 # Shells
  environment.sessionVariables = {		 # Session Variables
    # WLR_NO_HARDWARE_CURSORS = "1";	 # if cursor is not workign then set the below to 1
    NIXOS_OZONE_WL = "1";	 # Hint electron apps to use wayland
  };
  environment.variables = {
    MY_MONITOR_1_DECORATION = "monitor = DP-3, 2560x1440@144.0,";
  };
    # MY_MONITOR_2_DECORATION = "";

  # User account
  users.users.shiva = {
    isNormalUser = true;
    description = "shiva";
    extraGroups = [ "networkmanager" "wheel" ];
    # adding the openssh config here
    openssh.authorizedKeys.keys = [ "AAAAB3NzaC1yc2EAAAADAQABAAABgQCv0py1uj8lVQMegIWiV1vW5QEuo0rQHqTDsSg6bRgxJ/VhSmZLR430Oa2+rJFIqInmlFtYNoIXDbXYugKbyxPkqBfrkAsk4OicRTET9aTRSJ/cBGSN4zFK6fWgYnv6255jAPfXOdPXLjTpXJ2hTkO8nTSXAcDA+eGzBWv/WHxhxJ19c5MgnBqFAvQO3igfm0hzeKcrtzhziTIHXiWFpZic7C0FC4fvD9VcAO+o3Zt20X53idrhBsxm/r2whRVh7xII7EhTdaDZSaxyKYU6tvhA7xI2CvkLq8SsguW/FJS5bsF2zwiU/ECByyMLuNdeaY0hS2AbJaaKyJ5rnROMRK+0dxUwv5coJYSzVRQMesJUpbiUPng+5Zy5gqNx8WWullkpUxx0efK1njhzTbeFYEKPbRRE0Ot9KAdj3dhBQ9cmvAL8lBB2cyFh40uXFgIG5bSvrABE4fjR6jSTPAjq7vnCKg5q5dAHOKczo/QjyGQF0MLVyWlFIHDQcusyrZwTpm8=" ];
  };
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
  };

  # DESKTOP ENVIRONMENT 
  services.xserver.enable = true; 	# Enable the X11 windowing system.
  services.xserver.videoDrivers = ["amdgpu"]; # Make sure Xserver uses the amdgpu driver
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
    (nerdfonts.override{ fonts = ["JetBrainsMono"];})
  ];

  # NIX settings
  nix.settings.experimental-features = [ "nix-command" "flakes" ];	# enable flakes
  system.stateVersion = "23.11"; # DON'T CHANGE THIS
}

