let
  pkgs = import <nixpkgs> {};
in
  pkgs.runCommand "example" {
    buildInputs = [
      pkgs.haskellPackages.pandoc
      pkgs.texlive.combined.scheme-small
    ];
    example = ./example.md;
  } ''
    mkdir -p $out
    pandoc $example -o $out/example.pdf
  ''
