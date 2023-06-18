{ nixpkgs ? import (import ../nix/sources.nix {}).nixpkgs {}
}:
let
  nodeEnv = nixpkgs.callPackage ./nix/node/node-env.nix { };
  nodePackages = nixpkgs.callPackage ./nix/node/node-packages.nix {
    inherit nodeEnv;
  };
  nodeDependencies = (import ./nix/node { pkgs = nixpkgs; }).nodeDependencies;
in rec
{
  inherit nodePackages;
  site = nixpkgs.stdenv.mkDerivation {
    name = "itcss";
    src = ./.;
    installPhase = ''
      # Make sure we don't use an already built dist/.
      rm -rf dist node_modules

      ln -sf ${nodeDependencies}/lib/node_modules ./node_modules
      ./node_modules/gulp/bin/gulp.js build
      mv dist $out
    '';
  };
}
