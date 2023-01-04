# ITCSS implementation

This is an attempt at creating an HTML+CSS implementation of the design system
following the ITCSS approach (following a Figma file). That approached was used
in the [Smart design system](https://design.smart.coop/), and this is what I'm
trying to follow here.

To have a quick feedback loop during development, a Gulp file is provided. It
automatically rebuilds CSS from SCSS files and Pug templates upon changes, and
serves the result.

A `package.json` file is provided to install the required dependencies. Once
the dependencies are installed, it is possible to build or serve everything
with the `build.sh` and `serve.sh` scripts.

```
$ nix-shell -p nodejs
$ npm install
$ ./build.sh
$ ./serve.sh
```

You can then visit `http://127.0.0.1:3000` or
`http://127.0.0.1:3000/static-binaries.html`.
