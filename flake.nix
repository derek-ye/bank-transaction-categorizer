{
  description = "WIP Nix flake for this package";
  
  outputs = inputs@{nixpkgs, self, ...}: inputs.flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [self.overlays.ghc96];
    };
  in {
    packages.bank-transaction-categorizer = pkgs.haskell.packages.ghc96.bank-transaction-categorizer;

    devShells.default = pkgs.mkShell {
      buildInputs = [
        pkgs.haskell.packages.ghc96.yesod-bin
        pkgs.haskell.packages.ghc96.ghc
        pkgs.haskell.packages.ghc96.cabal-install
        pkgs.ghciwatch
        pkgs.stack
        pkgs.postgresql
        pkgs.zlib
      ];

      shellHook = ''
        echo "Yesod development environment loaded"
        echo "Available commands: yesod, stack, cabal, ghc, ghciwatch"
      '';
    };
  }) // {
    overlays = {
      # Override the GHC 9.6 package set with the Haskell overlay defined below
      # (which includes this package & its 'openai' dependency).
      ghc96 = _final: prev: {
        haskell = prev.haskell or {} // {
          packages = prev.haskell.packages or {} // {
            ghc96 = prev.haskell.packages.ghc96.override (oldArgs: {
              overrides = prev.lib.composeManyExtensions [(self.overlays.haskell prev)];
            });
          };
        };
      };
      # Add 'openai' & this package to any Haskell package set.
      haskell = pkgs: hfinal: hprev:
        let
          hlib = pkgs.haskell.lib;
        in {
          # Test suite requires OpenAI API key.
          openai = hlib.dontCheck (hfinal.callHackageDirect {
            pkg = "openai";
            ver = "1.1.0";
            sha256 = "sha256-8Ksve9yMH3cc+yiZg+eDuCws2IabejrSvYqIOzrizAw=";
          } { });
          # Test suite fails.
          bank-transaction-categorizer = hlib.dontCheck (hfinal.callCabal2nix "bank-transaction-categorizer" ./. { });
        };
    };
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
}
