

```
$ nix-shell -p nodejs
$ npm init # create the initial package.json
$ npm i --lockfile-version 2 mjml
$ node node_modules/mjml/bin/mjml test-email.mjml -o output.html
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
- Host the font ourselves, instead of using Google Fonts.
- There are interesting things listed here:
  https://documentation.mjml.io/#ports-and-language-bindings (e.g. a Rust
  version)
