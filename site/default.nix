{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  inherit (import ../default.nix {}) to-prefixed-html replace-md-links;
  to-html = src: to-prefixed-html "/design-system" "inter" src;
  app = (import ../release.nix).guide;

in rec
{
  md.nix = ./nix.md;
  md.nav-block = ./nav-block.md;

  html.nix = to-html md.nix;
  html.nav-block = to-html md.nav-block;
  html.all = pkgs.runCommand "all" {} ''
    mkdir $out

    cp ${html.nix} $out/nix.html
    cp ${html.nav-block} $out/nav-block.html
    ${pkgs.bash}/bin/bash ${replace-md-links} $out

    cp -r --no-preserve=mode \
      ${app}/share/doc/x86_64-linux-ghc-8.0.2/design-system-0.0.0/html $out/haddock
    rm $out/haddock/ocean.css
    find $out/haddock -maxdepth 1 -name '*.html' \
      -exec ${pkgs.bash}/bin/bash ${../scripts/change-haddock.sh} {} \;
  '';

  static = pkgs.runCommand "static" {} ''
    mkdir $out

    cp -r ${../static}/* $out/
  '';
}
