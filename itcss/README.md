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
```

The resulting files are in the `dist/` directory.

For development, another script is available:

```
$ ./serve.sh
```

You can then visit `http://127.0.0.1:3000` or
`http://127.0.0.1:3000/static-binaries.html`.

# Dimensions

In the Smart design system done by Mono, it seems the convention is to use 1rem
equals 10px. (And the pixel values in the browse maps to pixel values as
reported by Figma.) Furthermore they specify the default font size on the
`body` element, at 1.6rem.

I don't know why but they specify it with `font-size: 62.5%;` on the root
element (`html`), instead of with `font-size: 10px;`. Here is a probable
explanation:
https://www.aleksandrhovhannisyan.com/blog/62-5-percent-font-size-trick/

I think the original Next.js-based design keeps the default rem size, i.e.
16px.
