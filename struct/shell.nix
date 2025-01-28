let
  sources = import ../nix/sources.nix;
  overlays = import ../nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };

  binaries = nixpkgs.haskellPackages.slab;
in
  nixpkgs.mkShell {
    buildInputs = [
      (import sources.red).wrapped-binaries
      (import sources.slab).binaries
      nixpkgs.busybox # for httpd
      nixpkgs.dart-sass
    ];
    shellHook = ''
      source <(slab --bash-completion-script `which slab`)
    '';
  }
