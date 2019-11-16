# Hypered design system

This is a repository for the (early state) web design system for Hypered SPRL.

The current example content (located in the `docs/` directory) is raw HTML and
CSS (using Tachyons). It is visible at
[hypered.github.io/design-system](https://hypered.github.io/design-system/).

A component explorer based on [Storybook](https://storybook.js.org/) is visible
at
[storybook-static.andyngo1.now.sh](https://storybook-static.andyngo1.now.sh/).

The same components, but rendered using `blaze-html`, are visible at
[hypered.github.io/design-system/components/](https://hypered.github.io/design-system/components/).

A Next.js-based site to quickly experiment with the current design and new
components is visible at
[noteed-next.andyngo1.now.sh](https://noteed-next.andyngo1.now.sh/).

Interesting pages are:

- [sprint-2](https://noteed-next.andyngo1.now.sh/sprint-2)
- [blog](https://noteed-next.andyngo1.now.sh/blog)
- [blog post](https://noteed-next.andyngo1.now.sh/blog/starting-with-nixops-1)
- [not-os](https://noteed-next.andyngo1.now.sh/projects/not-os)
- [waveguide](https://noteed-next.andyngo1.now.sh/projects/waveguide)
- [nubs-bash](https://noteed-next.andyngo1.now.sh/nubs-bash)
- [nubs-bash/hello](https://noteed-next.andyngo1.now.sh/nubs-bash/hello)


## Next.js and Storybook

The following instructions can be followed with either a Docker container or a
Nix shell.

If using Docker:

```
$ docker run -it -v $(pwd):/src node bash -c 'cd /src ; <some command>'
```

If using Nix:

```
$ nix-shell -p nodejs -p yarn --run '<some command>'
```

Note: `npm` in the following instructions can be replaced by `yarn`.


### Dependencies

Install the dependencies using:

```
npm install
```

The result is a `node_modules` directory.


### Next.js telemetry

Next.js collects anonymous telemetry. This can be disabled with:

```
npx next telemetry disable
```

To check the status of Next.js telemetry:

```
npx next telemetry status
```


### Running the sites locally

To run the Next.js site locally, on port `3000`:

```
npm run dev
```

To run the Storybook.js site locally, on port `6006`:

```
npm run storybook
```


### Building static sites

To build the Next.js pages into the `out/` directory:

```
npm next build
npm next export
```

To build the Storybook.js pages into the `storybook-static/` directory:

```
npm run build-storybook
```


## Rendering React components from the command-line

A helper Node.js script is provided to render components from the command-line:

```
$ node render-components.js 
<footer class="pv4" data-reactroot="">
  <p class="inline-flex bt b--black-50 pt4 lh-copy">© Võ Minh Thu, 2017-2019.</p>
</footer>
```


## Notes

There is also a custom Revealjs template that (should) match the design-system.

The `docs/` directory is used because this repository can then use the GitHub
Pages feature from the `master` branch.


## Elsewhere

- The [IBM design system](https://www.carbondesignsystem.com/) is open source.
