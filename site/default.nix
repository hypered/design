{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  inherit (import ../.) to-prefixed-html replace-md-links;
  to-html = src: to-prefixed-html "" "ibm-plex" src;
  binaries = (import ../.).binaries;
  # TODO We need to update our change-haddock.sh script to use
  # this more recent derivation.
  haddock = (import ../.).haddock;
  struct = (import ../itcss {}).struct;

in rec
{
  md.index = ./index.md;
  md.nix = ./nix.md;
  md.nav-block = ./nav-block.md;

  html.index = to-html md.index;
  html.nix = to-html md.nix;
  html.nav-block = to-html md.nav-block;

  html.all = pkgs.runCommand "all" {} ''
    mkdir $out

    cp ${html.index} $out/index.html
    cp ${html.nix} $out/nix.html
    cp ${html.nav-block} $out/nav-block.html
    ${pkgs.bash}/bin/bash ${replace-md-links} $out

    #cp -r --no-preserve=mode \
    #  ${haddock}/share/doc/hypered-design-0.0.0/html \
    #  $out/haddock
  '';

  html.hs = pkgs.runCommand "hs" {
      buildInputs = [ pkgs.glibcLocales ];
    } ''
    export LANG="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"

    # Generate the guide

    mkdir -p generated/{min,pretty,static} $out
    ${binaries}/bin/hypered-design generate-guide
    mv generated/pretty $out/hs

    # Create the template and example pages

    mkdir -p docs/hs
    ${binaries}/bin/hypered-design generate-templates

    ${pkgs.haskellPackages.pandoc}/bin/pandoc \
      --standalone \
      --template generated/templates/default.html \
      --lua-filter ${../pandoc/tachyons.lua} \
      --output docs/hs/example--template.html \
      -M prefix="" \
      -M font="inter" \
      ${../pandoc/lua.md}

    ${pkgs.haskellPackages.pandoc}/bin/pandoc \
      --standalone \
      --template generated/templates/default.html \
      --lua-filter ${../pandoc/tachyons.lua} \
      --output docs/hs/example--template-ibm-plex.html \
      -M prefix="" \
      -M font="ibm-plex" \
      ${../pandoc/lua.md}

    ${pkgs.haskellPackages.pandoc}/bin/pandoc \
      --standalone \
      --template ${../pandoc/struct.html} \
      --output docs/hs/example--template-struct.html \
      -M prefix="" \
      ${../pandoc/struct.md}

    cp docs/hs/example--template.html $out/hs/
    cp docs/hs/example--template-ibm-plex.html $out/hs/
    cp docs/hs/example--template-struct.html $out/hs/
  '';

  # all + static, to serve locally with scripts/ghcid.sh
  html.all-with-static = pkgs.runCommand "all-with-static" {} ''
    mkdir $out
    cp -r ${html.all}/* $out/
    cp -r ${html.hs}/* $out/
    cp -r --no-preserve=mode ${static} $out/static
    cp -r ${struct}/static/css/struct $out/static/css/struct
    cp -r ${struct}/static/css/min/struct.css $out/static/css/struct.min.css
    cp -r ${struct}/static/fonts/* $out/static/fonts/
  '';

  static = pkgs.runCommand "static" {} ''
    mkdir -p out/{css,fonts,images,img}

    cp -r ${../static}/css/* out/css/
    cp -r ${../static}/fonts/* out/fonts/
    cp -r ${../static}/img/* out/img/

    cp -r ${struct}/static/css/* out/css/
    cp -r ${struct}/static/css/min/struct.css out/css/struct.min.css
    cp -r ${struct}/static/fonts/* out/fonts/
    cp -r ${struct}/static/images/* out/images/

    cp -r out $out
  '';
}
