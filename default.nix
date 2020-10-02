{ nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs {};
in rec
{
  template = pandoc/default.html;
  lua-filter = pandoc/tachyons.lua;
  to-prefixed-html = prefix: font: src: pkgs.runCommand "html" {} ''
    ${pkgs.pandoc}/bin/pandoc \
      --from markdown \
      --to html \
      --standalone \
      --template ${template} \
      -M prefix="${prefix}" \
      -M font="${font}" \
      --lua-filter ${lua-filter} \
      --output $out \
      ${src}
  '';
  to-html = src: to-prefixed-html "" "inter" src;
  replace-md-links = scripts/replace-md-links.sh;

  md.lua = pandoc/lua.md;

  pandoc-example = to-html md.lua;

  # This is bin/hypered-guide.hs.
  app = (import ./release.nix).guide;

  # This is the non-Next, non-Storybook static site. It contains
  # some rendered Markdown documentation and Haddock.
  site = (import site/default.nix {}).html.all;

  # This is the static directory, usually hosted as /static
  # on websites using this design system.
  static = (import site/default.nix {}).static;
}
