{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  inherit (import ../.) to-prefixed-html replace-md-links;
  to-html = src: to-prefixed-html "/design" "inter" src;
  app = (import ../release.nix).guide;
  # TODO We need to update our change-haddock.sh script to use
  # this more recent derivation.
  haddock = (import ../.).haddock;

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
      ${app}/share/doc/x86_64-linux-ghc-8.0.2/design-system-0.0.0/html \
      $out/haddock
    #cp -r --no-preserve=mode \
    #  ${haddock}/share/doc/design-system-0.0.0/html \
    #  $out/haddock
    rm $out/haddock/ocean.css
    find $out/haddock -maxdepth 1 -name '*.html' \
      -exec ${pkgs.bash}/bin/bash ${../scripts/change-haddock.sh} {} \;
  '';

  static = pkgs.runCommand "static" {} ''
    mkdir $out

    cp -r ${../static}/* $out/
  '';
}
