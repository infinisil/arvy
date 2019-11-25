{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/2436c27541b2f52deea3a4c1691216a02152e729";
    sha256 = "0p98dwy3rbvdp6np596sfqnwlra11pif3rbdh02pwdyjmdvkmbvd";
  }) { config = {}; overlays = []; }
}:
let

  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-small
      latexmk
      textpos
      forloop;
  };

in pkgs.stdenv.mkDerivation {
  name = "slides";
  src = pkgs.lib.sourceByRegex ./. [
    "figures.*"
    "Makefile"
    ".*\\.tex"
  ];
  nativeBuildInputs = [
    tex
  ];
  preBuild = ''
    HOME=$(mktemp -d)
  '';
  installPhase = ''
    install -Dt $out Thesis.pdf
  '';
}
