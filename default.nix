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

      arvy = super.callCabal2nix "arvy" src {};

      polysemy = (super.callCabal2nixWithOptions "polysemy" polysemySrc "--no-hpack" {
        th-abstraction = self.th-abstraction_0_3_1_0;
      }).overrideAttrs (old: {
        configureFlags = [ "-f -error-messages" ];
      });

      polysemy-plugin = super.callCabal2nixWithOptions "polysemy-plugin" "${polysemySrc}/polysemy-plugin" "--no-hpack" {
        inspection-testing = self.inspection-testing_0_4_2;
      };

      polysemy-zoo = super.callCabal2nixWithOptions "polysemy-zoo" (pkgs.fetchFromGitHub {
        owner = "isovector";
        repo = "polysemy-zoo";
        rev = "488c961945b9bbd64806199e5fe991f57f7e83ac";
        sha256 = "0q7n5h0sn8bm931pmlml6d6z7r7kyi238b6rhvkm81fq28zxhaya";
      }) "--no-hpack" {};

    });
  });

  pkg = hpkgs.arvy;

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [
      pkgs.haskellPackages.cabal-install
      pkgs.gnuplot
    ];
  });

in pkg // { inherit env; }
