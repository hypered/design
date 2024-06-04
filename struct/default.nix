{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:
let
in
{
  site = nixpkgs.stdenv.mkDerivation {
    name = "site";
    src = ./.;
    buildInputs = [
      (import ../../slab/default.nix).binaries
      nixpkgs.sass
      nixpkgs.glibcLocales
    ];
    installPhase = ''
      # Make sure we don't rely on an existing _site.
      rm -r _site

      export LANG="en_US.UTF-8"
      export LC_ALL="en_US.UTF-8"

      make -j 8
      mv _site $out
    '';
  };
}
