{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/61f0936d1cd73760312712615233cd80195a9b47";
    sha256 = "1fkmp99lxd827km8mk3cqqsfmgzpj0rvaz5hgdmgzzyji70fa2f8";
  }) {}
}:
let

  inherit (pkgs) lib;

  src = lib.sourceByRegex ./. [
    "lib.*"
    "LICENSE"
    "Setup.hs"
    "arvy.cabal"
  ];

  pkg = pkgs.haskell.packages.ghc864.callCabal2nix "arvy" src {};

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [ pkgs.haskellPackages.cabal-install ];
  });

in pkg // { inherit env; }
