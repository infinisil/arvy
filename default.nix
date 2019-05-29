{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/acbdaa569f4ee387386ebe1b9e60b9f95b4ab21b";
    sha256 = "0xzyghyxk3hwhicgdbi8yv8b8ijy1rgdsj5wb26y5j322v96zlpz";
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

  pkg = pkgs.haskellPackages.callCabal2nix "arvy" src {};

  env = pkg.env.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [] ++ [ pkgs.haskellPackages.cabal-install ];
  });

in pkg // { inherit env; }
