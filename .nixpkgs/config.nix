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
  packageOverrides = super: let self = super.pkgs; in with self; rec {

    ######################################################
    # Install these with `nix-env -iA nixos.my-essentials`
    ######################################################

    ## My one-shot install environment.
    my-meta-env = buildEnv {
      name = "meta-env";
      paths = [
        my-dev-tools
        my-essentials
        my-games
        my-haskell-env
        my-media
      ];
    };

    ## Haskell environment
    my-haskell-env = buildEnv {
      name = "haskell-env";
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
      ];
    };

    my-essentials = buildEnv {
      name = "essentials";
      paths = [
        arandr
        cifs-utils
        dmenu
        emacs
        encfs
        evince
        exfat
        firefox
        gnupg
        htop
        keepassx
        keychain
        maim
        networkmanagerapplet
        nix-repl
        pass
        qtpass
        rxvt_unicode-with-plugins
        slop
        thunderbird
        udisks2
        unzip
        upower
        wget
        which
        xscreensaver

        # For clipboard syncing
        xsel
        parcellite
        xdotool
      ];
    };

    my-python-data-science-env = buildEnv {
      name = "python-data-science-env";
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
      name = "games";
      paths = [
        unstable.crawlTiles
        steam
      ];
    };

    my-dev-tools = buildEnv {
      name = "dev-tools";
      paths = [
        ansible
        binutils
        git
        patchelf
        silver-searcher
        sqlite
        vagrant
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
      name = "keyboard-tools";
      paths = [
        dfu-util
      ];
    };

    my-media = buildEnv {
      name = "media";
      paths = [
        # Pulse audio control
        # pavucontrol

        spotify
        vlc
      ];
    };

    my-bluetooth = buildEnv {
      name = "bluetooth";
      paths = [
        blueman
      ];
    };

  };
}
