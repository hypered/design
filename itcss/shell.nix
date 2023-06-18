{ sources ? import ../nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  nodeEnv = pkgs.callPackage ./nix/node/node-env.nix { };
  nodePackages = pkgs.callPackage ./nix/node/node-packages.nix {
    inherit nodeEnv;
  };
in nodePackages.shell
