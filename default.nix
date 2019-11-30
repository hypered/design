{ nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs {};
in rec
{
  template = pandoc/default.html;
  lua-filter = pandoc/tachyons.lua;
  md.lua = pandoc/lua.md;

  pandoc-example = pkgs.runCommand "pandoc-example" {} ''
    mkdir $out
    ${pkgs.pandoc}/bin/pandoc \
      --standalone \
      --template ${template} \
      --lua-filter ${lua-filter} \
      --output $out/example--template.html \
      ${md.lua}
  '';
}
