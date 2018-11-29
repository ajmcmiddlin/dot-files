# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # Needed for corefonts
  nixpkgs.config.allowUnfree = true;

  # nixpkgs.overlays = [ (import /home/andrew/.config/nixpkgs/overlays) ];

  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/machine-specific.nix
    ];

  nix.trustedBinaryCaches = [
    "https://hydra.qfpl.io"
    "https://cache.nixos.org"
    "https://nixcache.reflex-frp.org"
  ];

  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "qfpl.io:xME0cdnyFcOlMD1nwmn6VrkkGgDNLLpMXoMYl58bz5g="
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  sound.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  hardware.pulseaudio = {
    enable = true;
    daemon.config = {
      # Allow app volumes to be set independently of master
      flat-volumes = "no";
    };
    # Get a lightweight package by default. Need full to support BT audio.
    # package = pkgs.pulseaudioFull;
  };


  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_AU.UTF-8";
  };

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts  # Micrsoft free fonts
      inconsolata  # monospaced
      liberation_ttf
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      terminus_font # for hidpi screens, large fonts
      ubuntu_font_family  # Ubuntu fonts
    ];
  };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # SEE .nixpkgs/config.nix for installed packages
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = (with pkgs; [
    exfat
    exfat-utils
    fuse_exfat

    pavucontrol
  ]);

  programs.fish.enable = true;
  programs.bash.enableCompletion = true;
  programs.ssh.startAgent = true;

  # Enable VirtualBox (don't install the package)
  virtualisation.virtualbox.host.enable = true;

  # virtualisation.docker.enable = true;

  # List services that you want to enable:

  # Enable yubikey
  services.pcscd.enable = true;

  # Blue light filter
  services.redshift = {
    enable = true;
    latitude = "-27.45817";
    longitude = "153.03443";
  };

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  # Enable upower service - used by taffybar's battery widget
  services.upower.enable = true;
  powerManagement.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Zero configuration DNS broadcast
  services.avahi.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    displayManager.slim.defaultUser = "andrew";
    # Try SLiM as the display manager
    # displayManager.lightdm.enable = true;
    xkbOptions = "ctrl:nocaps";

    windowManager.default = "xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages : [haskellPackages.taffybar];
    };

    # synaptics = {
    #   enable = true;
    #   twoFingerScroll = true;
    #   horizontalScroll = true;
    #   tapButtons = true;
    #   palmDetect = true;
    #   additionalOptions = ''
    #   Option            "VertScrollDelta"  "-111"
    #   Option            "HorizScrollDelta" "-111"
    #   '';
    # };

    libinput = {
      enable = true;
      naturalScrolling = true;
    };
  };

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.andrew = {
    createHome = true;
    extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "docker" "vboxusers" "input"];
    group = "users";
    home = "/home/andrew";
    isNormalUser = true;
    shell = pkgs.fish;
    uid = 1000;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQD703y9vkye+NwmswvGDCCdZqyrvE5yQTXq6S1GbNOsRe7LV5nkVsZfA5uKFa1Bhbl6lvSB4y0EgDzXCUNwm3SyqM0ClywOpt0A5Z6DFbMEnFUz0HVCN8fgbhnftX1Nu8NOWucuVVQi1jj4rkttR9MMWS4YJ2HZLVJ4b59C0Ko/hGzuzcvWsKPrZ/5jQWjdCIPVqqQ+2aSlH2ykmRwH+TpqMjjZSQi10MZYnzpnbbV3CFmLo6guPtylcrf52N/XsQo/WHyy53dzBGRt7LXekShOVIhBN4i4koTLIulnWrT0babHzhLwheBPw/1QN8kAB8ZdHB/3wWhAEqTJI7V8hUvqsilexjLx/q6s1hIYfM84i6LgWkjwzWEkBMv4ZRceYv5HKKNMi6460GquXwhv+rXv1ZiAvOL7Md9+i2oVqgVI3QQHsneskPqlyy4DKtkCvUBDl+Y1eqxcZChUU2SoJ7lAaDpJ8fVL3RRB36WQxU4Itjac1J5O91maqrK2lClXR1xSa6FjLjx40OlywRN5cdGdAgo9K/nJeoTgqfEvZKKC5j8VZJnTIPxlWmt26EOGbsgZgEmoTv9CThdVO3dOaWsFjqAWiO8DNfpaSMaWNsMp64dxT8lNtZsEMT0piF7OXUhivF30lpHjeF0j9Ab3F6XQIjBGkpDggYHal7VDmg/w5Q== andrew@stevie"

      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDEIyBpALOSXxw9d1maCBA3fQmwT1rc5jhPHXmLPlxmppV060EDfKqJyjjz0H5IqvqNbjMt2cI2plleoMjPvOY4oVwPq8xxAHKeS+iE4uLcW+N3piHeQxn0IV5RBLF2XMRZ3ZbhuM1TF5IzdmIr2mkFqbEjokmFY/fqeofD1JAEqa31xW9N4B5m4TR9HH6AMoBU8gWsVQk9UQEBUgpEUx9GYySKOuj3x+lWZANFxqAMKz4ABh0YTobL+TMeXiTghltPi9lzBO16VrKBzPNy0jNeUdmxyzqpCzUxoVtZZXLFVxPXv1HZqKgZurzbgE8/BielkIAJmUaYjvm5NOuIWolT4SmRfOT69QiUDYXcvLt7qew6IFBUUL3eQSXNB4gJENPLuPnVvtrE5Iey1YYBeTjTkLRwqvMVmqo9MuNYW4kWfxW/y10hv46Pu3JRwZX3SzXktGRkdJnPHDiAvnhGg9rP628gF2YUbXQHyeldpy/eysTBEDNZr6KMH3mxEVPNktDHazBzb1eu6aPIHyYIqxZEwJ8fbTPeqCrZHl845fT7yN8LlBPDV2lZ0d4wGcgf/HW5M1pVNlDJTROzwuIv2X4r7is8/uZHyC1HYQiUf6WMsIDtKavZbyxjh/034okdraTYB0YvObK02WdG3Yg1R20WBNzletSR8WTm0DNBZzX9MQ== andrew@hermes"
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
