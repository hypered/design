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
 
    buildPhase = ''
      pdflatex template
      biber template
      makeglossaries template
      pdflatex template
      pdflatex template
    '';
 
    installPhase = ''
      mkdir -p $out
      cp template.pdf $out/
    '';
  };
}
