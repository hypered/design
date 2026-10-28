{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:
let
  nodeModules = nixpkgs.importNpmLock.buildNodeModules {
    package = nixpkgs.lib.importJSON ./package.json;
    packageLock = nixpkgs.lib.importJSON ./package-lock.json;
    inherit (nixpkgs) nodejs;
  };
in rec
{
  inherit nodeModules;
  emails = nixpkgs.stdenv.mkDerivation {
    name = "emails";
    src = ./.;
    installPhase = ''
      # Make sure we don't use an already built dist/.
      rm -rf dist node_modules

      ln -sf ${nodeModules}/node_modules ./node_modules
      mkdir $out
      node_modules/mjml/bin/mjml example.mjml -o $out/example.html
    '';
  };
}
