# ITCSS implementation

This is an attempt at creating an HTML+CSS implementation of the design system
following the ITCSS approach (following a Figma file). That approached was used
in the [Smart design system](https://design.smart.coop/), and this is what I'm
trying to follow here.

To have a quick feedback loop during development, a Gulp file is provided. It
automatically rebuilds CSS from SCSS files and Pug templates upon changes, and
serves the result.

# Using `node_modules`

A `package.json` file is provided to install the required dependencies. Once
the dependencies are installed, it is possible to build or serve everything
with `gulp`.

```
$ nix-shell -p nodejs
$ npm install
$ ./node_modules/gulp/bin/gulp.js build
```

The resulting files are in the `dist/` directory.

To run the development server:

```
$ ./node_modules/gulp/bin/gulp.js
```

You can then visit `http://127.0.0.1:3002` or
`http://127.0.0.1:3002/examples/static-binaries.html`.

# Using Nix

Instead of running `npm install` and getting a `node_modules/` directory, we
can have a Nix shell populated by node2nix.

The script `node2nix.sh` is used to (re-)generate the necessary files in
`nix/node/`.

Building or serving are then just:

```
$ nix-shell
$ gulp build
$ gulp
```

Note: instead of using `npm install` initially, it seems we could use
https://www.npmjs.com/package/add-dependency.

Building the static site is also possible as an Nix attribute:

```
$ nix-build -A site
```

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

# PostCSS

PostCSS can be used to drive PurgeCSS and CSSNano. The resulting `main.min.css`
is not yet used.

```
$ npm install postcss postcss-cli @fullhuman/postcss-purgecss cssnano
$ node_modules/.bin/postcss dist/static/css/main.css -o dist/static/css/main.min.css
```

It is possible to modify the configuration file to take e.g. a single HTML file
into account, or remove the CSSNano step. In that case, the generated CSS is
easily diffable against the original `main.css` file (unless SASS output style
is set to `compressed` instead of `expanded`).

# Notes

Here are some notes about how the SCSS are written. This is may be trivial for
a web designer but anyway.

- We set 1rem to be 10px, and the default font size to be 1.6rem, i.e. 16px.
  See [Dimensions](#Dimensions) above.
- When comparing Figma to the implemented SCSS, 1px maps to 1px.
- Something like a `.c-navbar` has a specified height, so that even if it's
  empty, it takes the right space. This is easier to visualize a page
  organization.
