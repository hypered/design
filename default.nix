let
  sources = import ./nix/sources.nix;
  overlays = import ./nix/overlays.nix;
  nixpkgs = import sources.nixpkgs { inherit overlays; };
in rec
{
  # Previous design using Tachyons
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

  # New design, Struct
  template-new = pandoc/struct.html;
  to-prefixed-html-new = prefix: src: nixpkgs.runCommand "html" {} ''
    ${nixpkgs.pandoc}/bin/pandoc \
      --from markdown \
      --to html \
      --standalone \
      --template ${template-new} \
      -M prefix="${prefix}" \
      --output $out \
      ${src}
  '';
  to-html-new = src: to-prefixed-html-new "" src;

  md.lua = pandoc/lua.md;
  md.struct = pandoc/struct.md;

  docbook-example = (import docbook/default.nix {}).minimal;

  pandoc-example = to-html md.lua;
  pandoc-example-new = to-html-new md.struct;

  # Build with nix-build -A <attr>
  # binaries + haddock are also available as binaries.all.
  binaries = nixpkgs.haskellPackages.hypered-design;
  haddock = nixpkgs.haskellPackages.hypered-design.doc;

  # This is the non-Next, non-Storybook static site. It contains
  # some rendered Markdown documentation and Haddock.
  site = (import site/default.nix {}).html.all;
  hs = (import site/default.nix {}).html.hs;
  itcss = (import ./itcss {}).site;
  struct = (import ./itcss {}).struct;

  # This is the static directory, usually hosted as /static
  # on websites using this design system.
  static = (import site/default.nix {}).static;
  # all + static, to serve locally with scripts/ghcid.sh
  all-with-static = (import site/default.nix {}).html.all-with-static;

  # An example PDF brochure created with LaTeX.
  brochure = (import brochure/default.nix { inherit nixpkgs; }).brochure;

  # A shell to try out our binaries
  # Run with nix-shell default.nix -A shell
  shell = nixpkgs.mkShell {
    buildInputs = [
      binaries
      nixpkgs.nodejs
    ];
    shellHook = ''
      source <(hypered-design --bash-completion-script `which hypered-design`)
    '';
  };
}
