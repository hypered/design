{ nixpkgs ? import <nixpkgs> {}
}:

let pkgs = nixpkgs;
in rec
{
  brochure = pkgs.stdenvNoCC.mkDerivation {
    name = "brochure";
    src = ./.;

    buildInputs = [
      pkgs.biber
      (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          anyfontsize
          biblatex
          biblatex-apa
          booktabs
          caption
          catchfile
          changepage
          datatool # provides datatool-base
          enumitem
          etoolbox
          float
          fontaxes
          fontsize
          glossaries
          hardwrap
          marginnote
          mfirstuc
          pagecolor
          plex # provides e.g. plex-sans
          scheme-basic
          sidenotes
          textcase
          titlesec
          xcolor
          xfor
          xkeyval
          ;
      })
    ];

    buildPhase = ''
      TEX_FILENAME="example-full"

      pdflatex "$TEX_FILENAME"
      biber "$TEX_FILENAME"
      makeglossaries "$TEX_FILENAME"
      pdflatex "$TEX_FILENAME"
      pdflatex "$TEX_FILENAME"
    '';

    installPhase = ''
      mkdir -p $out
      cp example-full.pdf $out/
    '';
  };
}
