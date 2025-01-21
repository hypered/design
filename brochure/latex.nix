let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
  pkgs.texlive.combine {
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
  }
