{ pkgs }
:

# Adapted from https://gist.github.com/dhess/2ca7e5e836c52fd0187637a49ace04fb
let
  ihaskellSrc = pkgs.fetchFromGitHub {
    owner = "gibiansky";
    repo = "IHaskell";
    rev = "94dc8a59552be6c680a1dcaaad240074df166fce";
    sha256 = "0zng6khnnqmv2chl2r4hnc56yyikbvj5bqfcy7nia641s9mzvaa1";
  };

  haskellDataSciencePackages = haskellPkgs: with haskellPkgs; [
    Frames
    foldl
  ];

  unstable = import <unstable> {};
in
{
  # For steam
  allowUnfree = true;

  # Might need to get Netflix working
  # pkgs.config.chromium = { enableWideVine = true; };

  packageOverrides = super: let self = super.pkgs; in with self; rec {

    ######################################################
    # Install these with `nix-env -iA nixos.my-essentials`
    ######################################################

    ## My one-shot install environment.
    hermes-env = buildEnv {
      name = "hermes-env";
      paths = [
        my-dev-tools
        my-essentials
        my-haskell-env
        my-laptop-tools
        my-media
        my-games
        my-recording
      ];
    };

    stevie-env = buildEnv {
      name = "stevie-env";
      paths = [
        my-dev-tools
        my-essentials
        my-haskell-env
        my-laptop-tools
        my-media
      ];
    };

    ## Haskell environment
    my-haskell-env = buildEnv {
      name = "my-haskell-env";
      paths = with pkgs.haskellPackages; [
        apply-refact
        cabal2nix
        cabal-install
        ghc
        ghcid
        hasktags
        hlint
        hoogle
        stylish-haskell
        # TODO: re-enable this when it passes tests
        # unicode-show
      ];
    };

    my-essentials = buildEnv {
      name = "my-essentials";
      paths = [
        arandr
        aspell
        aspellDicts.en
        autossh
        calibre
        chromium
        cifs-utils
        dmenu
        encfs
        evince
        exfat
        feh
        firefox
        gimp
        gnupg
        htop
        inkscape
        keepassx
        keychain
        libreoffice
        maim
        gnome3.nautilus
        neovim
        networkmanagerapplet
        nix-prefetch-scripts
        nix-repl
        obs-studio
        # openconnect
        # openconnect_openssl
        owncloud-client
        pandoc
        pass
        qtpass
        rxvt_unicode-with-plugins
        samba
        signal-desktop
        silver-searcher
        slop
        haskellPackages.taffybar
        thunderbird
        tigervnc
        transmission-gtk
        udisks2
        unzip
        volumeicon
        wget
        which
        xclip
        xscreensaver
        yubioath-desktop

        # For clipboard syncing
        xsel
        parcellite
        xdotool
      ];
    };

    my-laptop-tools = buildEnv {
      name = "my-laptop-tools";
      paths = [
        powertop
        upower
        xorg.xbacklight
      ];
    };

    my-python-data-science-env = buildEnv {
      name = "my-python-data-science-env";
      paths = with pkgs.python3.pkgs; [
        jupyter
        matplotlib
        numpy
        pandas
        python
        scipy
      ];
    };

    my-games = buildEnv {
      name = "my-games";
      paths = [
        unstable.crawlTiles
        # openarena
        steam
      ];
    };

    my-dev-tools = buildEnv {
      name = "my-dev-tools";
      paths = [
        ansible
        binutils
        docker_compose
        emacs
        git
        git-crypt
        gnumake
        nixops
        patchelf
        postgresql
        silver-searcher
        sqlite-interactive
        vagrant
        virtualbox
        vscode
      ];
    };

    my-data-tools = buildEnv {
      name = "my-data-tools";
      paths = [
        qgis
        mdbtools
      ];
    };

    my-keyboard-tools = buildEnv {
      name = "my-keyboard-tools";
      paths = [
        dfu-util
      ];
    };

    my-media = buildEnv {
      name = "my-media";
      paths = [
        # Pulse audio control
        # pavucontrol

        mplayer
        ffmpeg
        spotify
        vlc
      ];
    };

    my-recording = buildEnv {
      name = "my-recording";
      paths = [
        ardour
        audacity

        jack2Full
        qjackctl
      ];
    };

    my-bluetooth = buildEnv {
      name = "my-bluetooth";
      paths = [
        blueman
      ];
    };

  };
}
