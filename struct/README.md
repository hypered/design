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

# Slab

I've started to write [Slab](https://slab-lang.org/). One goal is to be able to
generate similar HTML pages to what we're already doing with Pug.js (as driven
by the Gulpfile).

Instead of a Gulpfile to find the `.pug` source file and translate them to
HTML, a Makefile is provided.

```
$ nix-shell --run 'make -j 8 && make serve'
```
