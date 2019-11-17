# Hypered design system

The Hypered design system is a collection of components, templates, and other
bits of HTML and CSS to create a unified user experience across Hypered
projects.

Components are provided in multiple compatible forms: `Next.js`, `blaze-html`,
and static HTML snippets.

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
$ node render-components.js footer
<footer class="pv4" data-reactroot="">
  <p class="inline-flex bt b--black-50 pt4 lh-copy">© Võ Minh Thu, 2017-2019.</p>
</footer>
```


## Comparing Haskell and Node outputs

In addition of component names, the above script can process its stdin with
'pretty' to normalize some input HTML.

In the example below, we can verify the footer component is the same in Haskell
and Node.

```
$ nix-shell --run 'runghc bin/hypered-guide.hs footer' | nix-shell -p nodejs --run 'node render-components pretty'
<footer class="pv4">
  <p class="inline-flex bt b--black-50 pt4 lh-copy">© Võ Minh Thu, 2017-2019.</p>
</footer>
$ nix-shell -p nodejs --run 'node render-components footer'
<footer class="pv4">
  <p class="inline-flex bt b--black-50 pt4 lh-copy">© Võ Minh Thu, 2017-2019.</p>
</footer>
```


## Pandoc

A Pandoc template can be generated with the `bin/hypered-templates.hs` script.
The resulting file is versioned at `pandoc/default.html` for convenience.

To render Markdown files to HTML, Pandoc can be used as follow:

```
$ pandoc --template templates/default.html -M title=README README.md
```

This renders this `README.md` file as a standalone HTML page using the
`default.html` templates. It fills the template `$title$` variable with the
string `"README"`. (See
https://pandoc.org/MANUAL.html#using-variables-in-templates for details.)

To use the templates provided by this design system, some additional care
should be taken, for instance to add the necessary Tachyons classes to headers
(`h1`, `h2`, ...).

Here is how the provided `pandoc/lua.md` example is rendered:

```
$ pandoc \
  --standalone \
  --template pandoc/default.html \
  --lua-filter pandoc/tachyons.lua \
  --output docs/components/example--template.html \
  pandoc/lua.md
```

Note that the top horizontal navigation component is filled by using the
document YAML metadata block (which can also be provided as a separate file).


## Nix

A `default.nix` file is provided to show how to render the above Pandoc example
with Nix.


## Notes

There is also a custom Revealjs template that (should) match the design-system.

The `docs/` directory is used because this repository can then use the GitHub
Pages feature from the `master` branch.


## Elsewhere

- The [IBM design system](https://www.carbondesignsystem.com/) is open source.
