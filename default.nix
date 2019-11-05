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
      framed;
  };

in pkgs.stdenv.mkDerivation {
  name = "thesis";
  src = pkgs.lib.sourceByRegex ./. [
    "figures.*"
    ".*\\.tex"
    ".*\\.cls"
    ".*\\.bst"
    ".*\\.bib"
  ];
  nativeBuildInputs = [
    tex
    pkgs.python3.pkgs.pygments
    pkgs.which
  ];
  buildPhase = ''
    HOME=$(mktemp -d)
    pdflatex -shell-escape Thesis.tex
    bibtex Thesis
    pdflatex -shell-escape Thesis.tex
    pdflatex -shell-escape Thesis.tex
  '';
  installPhase = ''
    mkdir -p $out
    mv Thesis.pdf $out
  '';
}
