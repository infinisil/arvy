{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/7815c86c104a99417db844791dcda34fe7a7965f";
    sha256 = "0k6ws2b2b6vrvq2g5h8fi8qscb0wk0wy097cnf36f9acd126k43j";
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

  polysemySrc = pkgs.fetchFromGitHub {
    owner = "isovector";
    repo = "polysemy";
    rev = "b5d086b85999708d1da98f5d3f7aa5f7067bc8a8";
    sha256 = "06wchcz0wk8hx85i8412zw8cb74hrm22nphxj5sg4038f3ql7i2j";
  };

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

      th-abstraction = super.th-abstraction_0_3_1_0;

      polysemy = (super.callCabal2nixWithOptions "polysemy" polysemySrc "--no-hpack" {}).overrideAttrs (old: {
        configureFlags = [ "-f -error-messages" ];
      });

      polysemy-plugin = super.callCabal2nixWithOptions "polysemy-plugin" "${polysemySrc}/polysemy-plugin" "--no-hpack" {
        inspection-testing = self.inspection-testing_0_4_2;
      };

      polysemy-zoo = super.callCabal2nixWithOptions "polysemy-zoo" (pkgs.fetchFromGitHub {
        owner = "isovector";
        repo = "polysemy-zoo";
        rev = "152401ba522b3182f35121100dc56ea25fea1bc3";
        sha256 = "01jifzd182212hdb075196y76sqk69rczhxwdi21j2p7ycwq7fxp";
      }) "--no-hpack" {};

      polysemy-RandomFu = hlib.doJailbreak (super.callHackageDirect {
        pkg = "polysemy-RandomFu";
        ver = "0.2.0.0";
        sha256 = "0yax8mzcw0gvy54fxzh433rw6dzc19j4wv3gdn83fm8jlrx8jqjn";
      } {});

    });
  });

  pkg = hpkgs.arvy;

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.gnuplot
    ];
  });

in pkg // { inherit env pkgs hpkgs; }
