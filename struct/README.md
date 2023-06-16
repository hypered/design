Yet another approach to a design system.

- The initial work was done with Tahyons and Next.js (and Storybook to show
  expose the components and examples).
- A second approach uses ITCSS and BEM, similarly to the Mono company (in the
  `../itcss/` directory).
- Here we follow ideas from `utopia.fyi`, `every-layout.dev`, the `.flow`
  class, ...

We re-use the `gulpfile.js` from `../itcss/` to build the SCSS and Pug
templates.

```
$ ./build.sh
$ ls dist/
```
