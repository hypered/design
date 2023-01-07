{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  inherit (import ../.) to-prefixed-html replace-md-links;
  to-html = src: to-prefixed-html "/design" "inter" src;
  binaries = (import ../.).binaries;
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

    #cp -r --no-preserve=mode \
    #  ${haddock}/share/doc/design-system-0.0.0/html \
    #  $out/haddock
  '';

  html.hs = pkgs.runCommand "hs" {
      buildInputs = [ pkgs.glibcLocales ];
    } ''
    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"

    # Generate the guide

    mkdir -p generated/{min,pretty,static} $out
    ${binaries}/bin/hypered-guide $@
    mv generated/pretty $out/hs

    # Create the template and example pages

    mkdir -p docs/hs
    ${binaries}/bin/hypered-templates

    ${pkgs.haskellPackages.pandoc}/bin/pandoc \
      --standalone \
      --template generated/templates/default.html \
      --lua-filter ${../pandoc/tachyons.lua} \
      --output docs/hs/example--template.html \
      -M prefix="/design" \
      -M font="inter" \
      ${../pandoc/lua.md}

    ${pkgs.haskellPackages.pandoc}/bin/pandoc \
      --standalone \
      --template generated/templates/default.html \
      --lua-filter ${../pandoc/tachyons.lua} \
      --output docs/hs/example--template-ibm-plex.html \
      -M prefix="/design" \
      -M font="ibm-plex" \
      ${../pandoc/lua.md}

    cp docs/hs/example--template.html $out/hs/
    cp docs/hs/example--template-ibm-plex.html $out/hs/
  '';

  static = pkgs.runCommand "static" {} ''
    mkdir $out

    cp -r ${../static}/* $out/
  '';
}
