let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
{
  template = pandoc/default.html;
  lua-filter = pandoc/tachyons.lua;
  to-prefixed-html = prefix: font: src: nixpkgs.runCommand "html" {} ''
    ${nixpkgs.pandoc}/bin/pandoc \
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

  docbook-example = (import docbook/default.nix {}).minimal;
  pandoc-example = to-html md.lua;

  # This is bin/hypered-guide.hs, compiled with an old reesd-stack (and
  # producing the Haddock expected by our scripts (that need to be updated)).
  app = (import ./release.nix).guide;

  # Build with nix-build -A <attr>
  # binaries + haddock are also available as binaries.all.
  binaries = nixpkgs.haskellPackages.hypered-design;
  haddock = nixpkgs.haskellPackages.hypered-design.doc;

  # This is the non-Next, non-Storybook static site. It contains
  # some rendered Markdown documentation and Haddock.
  site = (import site/default.nix {}).html.all;

  # This is the static directory, usually hosted as /static
  # on websites using this design system.
  static = (import site/default.nix {}).static;
}
