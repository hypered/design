This is an attempt at creating an HTML+CSS implementation of the design system
following the ITCSS approach. That approached was used in the [Smart design
system](https://design.smart.coop/), and this is what I'm trying to follow
here.

To have a quick feedback loop during development, a Gulp file is provided. It
automatically rebuild CSS from SCSS files and Pug templates upon changes, and
serves the result.

I used `nix-shell -p nodejs` then manually installed some dependencies. They
ended up into `../node_modules` (I guess this is because there is already a
`../packages.json` file) instead of being in this directory. It also means that
this changes the `packages.json` file but I don't want to commit those changes.

```
$ nix-shell -p nodejs
$ npm install gulp
$ npm install gulp-pug
$ npm install gulp-sass
$ npm install browser-sync
$ npm install sass
```

Once the dependencies are installed, it is possible to build or serve
everything with the `build.sh` and `serve.sh` scripts.
