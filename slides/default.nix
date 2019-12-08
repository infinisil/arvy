{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/2436c27541b2f52deea3a4c1691216a02152e729";
    sha256 = "0p98dwy3rbvdp6np596sfqnwlra11pif3rbdh02pwdyjmdvkmbvd";
  }) { config = {}; overlays = []; }
}:
let

  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-small
      algorithmicx
      cm-super
      algorithms
      todonotes
      minted
      fvextra
      ifplatform
      xstring
      framed
      commath
      pgfplots
      latexmk
      numprint
      textpos
      forloop
      multirow
      tabu
      biblatex
      logreq
      biber;
  };

in pkgs.stdenv.mkDerivation {
  name = "slides";
  src = pkgs.lib.sourceByRegex ./. [
    "Makefile"
    ".*\\.tex"
    ".*\\.bib"
  ];
  postUnpack = "cp -r ${../data} data";
  nativeBuildInputs = [
    tex
  ];
  preBuild = ''
    HOME=$(mktemp -d)
  '';
  installPhase = ''
    install -Dt $out presentation.pdf
  '';
}
