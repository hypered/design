# HTML emails

We use MJML to generate HTMLs. The example email should closely match the
[reference email](https://hypered.design/prototypes/refli/email.html).

- We include the font weight (600) necessary for the title (it uses Semi Bold).
- We set the font size for the title and the paragraphs to match the
  rendering of the reference email, when display on a 2560x1440 laptop
  (but with high DPI scaling applied, so I think this matches 1280x720).
- We also set the line height for paragraphs.

There differences that we could fix (e.g. the size of the logo), although since
we use a fluid design, matching the rendering (on e.g. a specific laptop) is
arbitrary. (The mail is not using a fluid design.)

There are differences that may be harder to fix. For instance the mail is
vertically centered. We could try to change that, with the risk that it would
break in some email clients.

# Development

```
$ nix-shell -p nodejs
$ npm init # create the initial package.json
$ npm i --lockfile-version 2 mjml
$ node node_modules/mjml/bin/mjml example.mjml -o example.html
```

The content of the input file can be created with
`https://mjml.io/try-it-live/`.

```
$ rm -r node_modules
$ ../itcss/node2nix.sh
$ nix-build -A emails --no-out-link
```

# Notes

- Maybe add a `dir` attribute on the `<mjml>` element.
- Host the font ourselves, instead of using Google Fonts?
- We're using a PNG logo. It seems the SVG wasn't visible in gmail.
- There are interesting things listed here:
  https://documentation.mjml.io/#ports-and-language-bindings (e.g. a Rust
  version)
