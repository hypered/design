{ nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs {};
in rec
{
  template = pandoc/default.html;
  lua-filter = pandoc/tachyons.lua;
  to-html = src: pkgs.runCommand "html" {} ''
    ${pkgs.pandoc}/bin/pandoc \
      --from markdown \
      --to html \
      --standalone \
      --template ${template} \
      --lua-filter ${lua-filter} \
      --output $out \
      ${src}
  '';
  replace-md-links = scripts/replace-md-links.sh;

  md.lua = pandoc/lua.md;

  pandoc-example = to-html md.lua;
}
