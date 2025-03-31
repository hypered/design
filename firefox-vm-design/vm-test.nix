let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};

  hosts = nodes: ''
    ${nodes.server.networking.primaryIPAddress} hypered.design
  '';

in nixpkgs.nixosTest {
  name = "firefox-vm/text";
  enableOCR = true;
  nodes = {
    server = { ... }: {
      imports = [
        ../modules/nginx.nix
        "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ];
      networking.firewall.enable = false;
    };
    client = { nodes, ... }: {
      imports = [
        ../../firefox-vm/machine/configuration.nix
        "${toString sources.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      ];
      networking.extraHosts = hosts nodes;
    };
  };

  # This can't work because the machine doesn't have network access,
  # unless we disable the sandbox.
  testScript = ''
    import time;
    start_all();
    client.wait_for_unit("display-manager");
    client.wait_for_x();
    client.wait_for_text("Unable to connect");

    url_and_names = [
      ("http://hypered.design", "homepage"),
      ("http://hypered.design/struct.html", "struct"),

      ("http://hypered.design/notes/preface.html", "notes-preface"),
      ("http://hypered.design/notes/foundations.html", "notes-foundations"),
      ("http://hypered.design/notes/technicalities.html", "notes-technicalities"),
      ("http://hypered.design/notes/media-queries.html", "notes-media-queries"),
      ("http://hypered.design/notes/dimensions.html", "notes-dimensions"),
      ("http://hypered.design/notes/fonts.html", "notes-fonts"),
      ("http://hypered.design/notes/scales.html", "notes-scales"),
      ("http://hypered.design/notes/flow.html", "notes-flow"),
      ("http://hypered.design/notes/placeholders.html", "notes-placeholders"),
      ("http://hypered.design/notes/layouts.html", "notes-layouts"),

      ("http://hypered.design/specimens/overview.html", "specimens-overview"),

      ("http://hypered.design/specimens/type-scale.html", "specimens-type-scale"),
      ("http://hypered.design/specimens/type-scale-a.html", "specimens-type-scale-a"),
      ("http://hypered.design/specimens/type-scale-b.html", "specimens-type-scale-b"),
      ("http://hypered.design/specimens/type-scale-c.html", "specimens-type-scale-c"),
      ("http://hypered.design/specimens/type-scale-d.html", "specimens-type-scale-d"),
      ("http://hypered.design/specimens/lorem.html", "specimens-lorem"),
      ("http://hypered.design/specimens/limits.html", "specimens-limits"),
      ("http://hypered.design/specimens/placeholders.html", "specimens-placeholders"),
      ("http://hypered.design/specimens/flexbox.html", "specimens-flexbox"),
      ("http://hypered.design/specimens/layout-bar.html", "specimens-layout-bar"),
      ("http://hypered.design/specimens/key-values.html", "specimens-key-values"),
      ("http://hypered.design/specimens/tables.html", "specimens-tables"),
      ("http://hypered.design/specimens/button.html", "specimens-button"),
      ("http://hypered.design/specimens/buttons.html", "specimens-buttons"),
      ("http://hypered.design/specimens/navigation.html", "specimens-navigation"),
      ("http://hypered.design/specimens/cover.html", "specimens-cover"),
      ("http://hypered.design/specimens/cover-content.html", "specimens-cover-content"),
      ("http://hypered.design/specimens/sidemenu.html", "specimens-sidemenu"),
      ("http://hypered.design/specimens/form.html", "specimens-form"),
      ("http://hypered.design/specimens/fill-int.html", "specimens-fill-int"),
      ("http://hypered.design/specimens/invoke--form.html", "specimens-invoke--form"),
      ("http://hypered.design/specimens/invoke-result.html", "specimens-invoke-result"),
      ("http://hypered.design/specimens/invoke--result-only.html", "specimens-invoke--result-only"),
      ("http://hypered.design/specimens/invoke.html", "specimens-invoke"),
      ("http://hypered.design/specimens/invoke--htmx.html", "specimens-invoke--htmx"),
      ("http://hypered.design/specimens/invoke--glsl.html", "specimens-invoke--glsl"),
      ("http://hypered.design/specimens/dropdown.html", "specimens-dropdown"),
      ("http://hypered.design/specimens/tachyons.html", "specimens-tachyons"),
      ("http://hypered.design/specimens/logo.html", "specimens-logo"),
      ("http://hypered.design/specimens/image.html", "specimens-image"),
      ("http://hypered.design/specimens/image-limit.html", "specimens-image-limit"),
      ("http://hypered.design/specimens/pikchr.html", "specimens-pikchr"),
      ("http://hypered.design/specimens/wireframe.html", "specimens-wireframe"),
      ("http://hypered.design/specimens/layout-lang.html", "specimens-layout-lang"),
      ("http://hypered.design/specimens/code.html", "specimens-code"),

      ("http://hypered.design/prototypes/refli/index-root.html", "refli-index-root"),
      ("http://hypered.design/prototypes/refli/index.html", "refli-index"),
      ("http://hypered.design/prototypes/refli/index-constant.html", "refli-index-constant"),
      ("http://hypered.design/prototypes/refli/login.html", "refli-login"),
      ("http://hypered.design/prototypes/refli/signup.html", "refli-signup"),
      ("http://hypered.design/prototypes/refli/subscribe.html", "refli-subscribe"),
      ("http://hypered.design/prototypes/refli/form-foo.html", "refli-form-foo"),
      ("http://hypered.design/prototypes/refli/confirm-foo-valid.html", "refli-confirm-foo-valid"),
      ("http://hypered.design/prototypes/refli/confirm-foo-errors.html", "refli-confirm-foo-errors"),
      ("http://hypered.design/prototypes/refli/message-foo-success.html", "refli-message-foo-success"),
      ("http://hypered.design/prototypes/refli/message-signup-success.html", "refli-message-signup-success"),
      ("http://hypered.design/prototypes/refli/message-subscribe-success.html", "refli-message-subscribe-success"),
      ("http://hypered.design/prototypes/refli/reset.html", "refli-reset"),
      ("http://hypered.design/prototypes/refli/select-password.html", "refli-select-password"),
      ("http://hypered.design/prototypes/refli/describe.html", "refli-describe"),
      ("http://hypered.design/prototypes/refli/describe-result.html", "refli-describe-result"),
      ("http://hypered.design/prototypes/refli/document.html", "refli-document"),
      ("http://hypered.design/prototypes/refli/blog-index.html", "refli-blog-index"),
      ("http://hypered.design/prototypes/refli/blog-post-introducing-refli.html", "refli-blog-post-introducing-refli"),
      ("http://hypered.design/prototypes/refli/blog-post-introducing-refli-fr.html", "refli-blog-post-introducing-refli-fr"),
      ("http://hypered.design/prototypes/refli/blog-post-introducing-refli-nl.html", "refli-blog-post-introducing-refli-nl"),
      ("http://hypered.design/prototypes/refli/http-400.html", "refli-http-400"),
      ("http://hypered.design/prototypes/refli/http-401.html", "refli-http-401"),
      ("http://hypered.design/prototypes/refli/http-404.html", "refli-http-404"),
      ("http://hypered.design/prototypes/refli/http-500.html", "refli-http-500"),
      ("http://hypered.design/prototypes/refli/http-502.html", "refli-http-502"),
      ("http://hypered.design/prototypes/refli/http-504.html", "refli-http-504"),
      ("http://hypered.design/prototypes/refli/email.html", "refli-email"),                           

      ("http://hypered.design/prototypes/refli/motherboard-index.html", "motherboard-motherboard-index"),
      ("http://hypered.design/prototypes/refli/motherboard-index-1.html", "motherboard-motherboard-index-1"),
      ("http://hypered.design/prototypes/refli/motherboard-index-2.html", "motherboard-motherboard-index-2"),
      ("http://hypered.design/prototypes/refli/motherboard-index-dense.html", "motherboard-index-dense"),
      ("http://hypered.design/prototypes/refli/motherboard-index-data.html", "motherboard-motherboard-index-data"),
      ("http://hypered.design/prototypes/refli/motherboard-document.html", "motherboard-motherboard-document"),     
    ]

    # We use sleep, that's really ugly but
    # - Using the OCR is super slow, and it doesn't seem to work on some pages.
    # - Or it's additional work to identify a word on a page that was not yet
    #   present on the previous page.
    # - At some point we'll be able to better drive the browser directly.
    #   (We currently use xdotool instead of e.g. webdriver bidi.)
    for (url, name) in url_and_names:
      client.succeed(f"firefox-control {url}")
      time.sleep(2);
      client.screenshot(name);
  '';
}
