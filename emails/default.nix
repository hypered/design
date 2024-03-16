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
  emails = nixpkgs.stdenv.mkDerivation {
    name = "emails";
    src = ./.;
    installPhase = ''
      # Make sure we don't use an already built dist/.
      rm -rf dist node_modules

      ln -sf ${nodeDependencies}/lib/node_modules ./node_modules
      mkdir $out
      node_modules/mjml/bin/mjml example.mjml -o $out/example.html
    '';
  };
}
