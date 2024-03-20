let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
  pkgs.runCommand "dummy" {
    buildInputs = [
      pkgs.biber
      # pkgs.mupdf
      (import ./latex.nix)
    ];
  } ""
