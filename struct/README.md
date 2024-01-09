# Struct

This is yet another approach to a design system for Hypered.

- The initial work was done with Tachyons and Next.js (and Storybook to expose
  the components and examples).
- A second approach uses ITCSS and BEM, similarly to the Mono company (in the
  `../itcss/` directory).
- Here we follow ideas from `utopia.fyi`, `every-layout.dev`, the `.flow`
  class, ...

In addition of a main CSS file, we can use specific files in "Specimens" to
demonstrate specific things (e.g. only the Reset CSS).

# Using Nix

We re-use the `gulpfile.js` from `../itcss/` to build the SCSS and Pug
templates. (So the structure of this directory is similar too.)

```
$ nix-shell ../itcss/shell.nix
$ gulp build --gulpfile ../itcss/gulpfile.js --cwd .
$ ls dist/
```

Building the static site is also possible as an Nix attribute:

```
$ nix-build --/itcss -A struct
$ nix-shell -p busybox --run "httpd -f -p 9000 -h result/"
```
