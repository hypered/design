let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
  pkgs.runCommand "dummy" {
    buildInputs = [
      pkgs.biber
      pkgs.mupdf
      (pkgs.texlive.combine {
        inherit (pkgs.texlive)
          anyfontsize
          biblatex
          biblatex-apa
          booktabs
          caption
          changepage
          datatool # provides datatool-base
          enumitem
          etoolbox
          float
          fontaxes
          fontsize
          glossaries
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
  } ""
