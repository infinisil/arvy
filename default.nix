{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/c4fec1c6314c0c9c7af59bb465a17d1950ec7464";
    sha256 = "1w8wjvmsap0jn4gq2gg76yphsgvl6a9v5vsnkjr0jzda1q83zw4h";
  }) {}
}:
let

  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  src = lib.sourceByRegex ./. [
    "lib.*"
    "eval.*"
    "app.*"
    "tests.*"
    "LICENSE"
    "Setup.hs"
    "arvy.cabal"
  ];

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {

      arvy = (hlib.overrideCabal (super.callCabal2nix "arvy" src {}) {
        doHaddock = true;
      }).overrideAttrs (old: {
        # Cabal currently can't generate docs for internal libraries depending on the non-internal library,
        # so we only generate docs for the non-internal library until that is fixed
        # See https://github.com/haskell/cabal/pull/5253#issuecomment-507069589
        haddockPhase = builtins.replaceStrings ["haddock --html"] ["haddock lib:arvy --html"] old.haddockPhase;
      });

      # https://github.com/snowleopard/alga/pull/210
      algebraic-graphs = hlib.dontCheck (hlib.unmarkBroken super.algebraic-graphs);

      th-abstraction = self.th-abstraction_0_3_1_0;
      th-lift = self.th-lift_0_8_0_1;

      polysemy = hlib.unmarkBroken super.polysemy;
      polysemy-plugin = hlib.dontCheck (hlib.unmarkBroken super.polysemy-plugin);
      polysemy-zoo = hlib.dontCheck (hlib.unmarkBroken super.polysemy-zoo);
      polysemy-RandomFu = hlib.unmarkBroken super.polysemy-RandomFu;

      type-errors = (hlib.unmarkBroken super.type-errors).override {
        first-class-families = self.first-class-families_0_5_0_0;
      };

    });
  });

  pkg = hpkgs.arvy;

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.gnuplot
      pkgs.ghostscript
    ];
  });

in pkg // { inherit env pkgs hpkgs; }
