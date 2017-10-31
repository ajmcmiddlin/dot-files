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
  packageOverrides = super: let self = super.pkgs; in with self; rec {

    ## My one-shot install environment.
    meta-env = buildEnv {
      name = "meta-env";
      paths = [
        haskell-env
      ];
    };

    ## Haskell environment
    my-haskell-env = buildEnv {
      name = "haskell-env";
      paths = with pkgs.haskellPackages; [
        ghcid
        stylish-haskell
        hlint
        hasktags
        hoogle
      ];
    };

    python-data-science-env = buildEnv {
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
      paths = [ unstable.crawlTiles ];
    };

    my-dev-tools = buildEnv {
      name = "dev-tools";
      paths = [ patchelf binutils ];
    };
  };
}
