{ nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs {};
in rec
{
  template = pandoc/default.html;
  lua-filter = pandoc/tachyons.lua;
  to-prefixed-html = prefix: src: pkgs.runCommand "html" {} ''
    ${pkgs.pandoc}/bin/pandoc \
      --from markdown \
      --to html \
      --standalone \
      --template ${template} \
      -M prefix="${prefix}" \
      --lua-filter ${lua-filter} \
      --output $out \
      ${src}
  '';
  to-html = src: to-prefixed-html "" src;
  replace-md-links = scripts/replace-md-links.sh;

  md.lua = pandoc/lua.md;

  pandoc-example = to-html md.lua;

  site = (import site/default.nix {}).html.all;
}
