# Hypered design system

The Hypered design system is a collection of components, templates, and other
bits of HTML and CSS to create a unified user experience across Hypered
projects. It also contains non-web implementations, such as LaTeX templates.

Components are provided in multiple compatible forms. The first one to exist
was the `Next.js` + Tachyons.

- `Next.js` + Tachyons. This is currently regarded as the reference
  implementation, until the ITCSS version is mature enough. This is the one
  re-implemented by other approaches.
- Pugs + ITCSS. This should be the reference implementation at some point,
  especially as it is created alongside a Figma file (which should be the
  source of truth).
- `blaze-html` (Haskell)
- static HTML snippets
- Pandoc templates
- Scripts to modify Haddock results
- XML + XSLT stylesheet (see [README](/xslt/README.md))
- ...

The main site built with Next.js is visible at
[hypered.github.io/design-system](https://hypered.github.io/design-system/). It
also contains a [component
explorer](https://hypered.github.io/design-system/storybook/) based on
[Storybook](https://storybook.js.org/).

The same components, but rendered using `blaze-html`, are visible at
[hypered.github.io/design-system/hs/](https://hypered.github.io/design-system/hs/).


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
$ npm install
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
$ yarn run dev
```

To run the Storybook.js site locally, on port `6006`:

```
npm run storybook
```


### Building static sites

The main website, just like the components, is written in Next.js in the
`pages/` directory. To build the Next.js pages into the `out/` directory:

```
npm run build
npm run export
```

One way to serve the directory locally is:

```
nix-shell -p busybox --run "httpd -f -p 9000 -h out/"
```

To build the Storybook.js pages into the `storybook-static/` directory:

```
npm run build-storybook
```

Again, one way to serve the directory locally is:

```
nix-shell -p busybox --run "httpd -f -p 9000 -h storybook-static/"
```


## Rendering React components from the command-line

A helper Node.js script is provided to render components from the command-line:

```
$ node render-components footer
<footer>
  <hr class="bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4" />
  <p class="inline-flex lh-copy">© Hypered, 2019.</p>
</footer>
```


## Comparing Haskell and Node outputs

In addition of component names, the above script can process its stdin with
'pretty' to normalize some input HTML.

In the example below, we can verify the footer component is the same in Haskell
and Node.

```
$ nix-shell --run 'runghc bin/hypered-guide.hs footer' \
  | nix-shell -p nodejs --run 'node render-components pretty'
<footer>
  <hr class="bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4">
  <p class="inline-flex lh-copy">© Hypered, 2019.</p>
</footer>
```

Notice that blaze-html writes `<hr>` instead of the `<hr />` obtained with the
Node.js script.


## ITCSS

The ITCSS approach is in the [`itcss/`](/itcss/README.md) directory.


## Pandoc template

A Pandoc template can be generated with the `bin/hypered-templates.hs` script.
The resulting file is versioned at [`pandoc/default.html`](pandoc/default.html)
for convenience.

To render Markdown files to HTML, Pandoc can be used as follow:

```
$ pandoc --template templates/default.html -M title=README -M font=inter README.md
```

This renders this `README.md` file as a standalone HTML page using the
`default.html` templates. It fills the template `$title$` variable with the
string `"README"`. (See
https://pandoc.org/MANUAL.html#using-variables-in-templates for details.)

To use the templates provided by this design system, some additional care
should be taken, for instance to add the necessary Tachyons classes to headers
(`h1`, `h2`, ...).

[Here](https://hypered.github.io/design-system/hs/example--template.html) is
how the provided [`pandoc/lua.md`](pandoc/lua.md) example is rendered:

```
$ pandoc \
  --standalone \
  --template pandoc/default.html \
  --lua-filter pandoc/tachyons.lua \
  --output docs/components/example--template.html \
  -M prefix="" \
  -M font=inter \
  pandoc/lua.md
```

Note that the top horizontal navigation component is filled by using the
document YAML metadata block (which can also be provided as a separate file).

The same document is rendered again
[Here](https://hypered.github.io/design-system/hs/example--template-ibm-plex.html)
with a different font.


## Pandoc + pdflatex PDF example

In the `pandoc-pdflatex/` directory, run:

```
$ nix-build
```

The resulting PDF file is then `result/example.pdf`.


## LaTeX-based brochure template

In the `brochure/` directory, run:

```
$ ./build.sh
```

The resulting PDF file is then `template.pdf`.


## Nix

A `default.nix` file is provided to show how to render the above Pandoc example
with Nix.


## Generated content

Some code is generated by scripts and committed for convenience:

- `scripts/create-stories-hs` is used to generate `Hypered/Stories.hs`,
- which is imported in the `bin/hypered-guide.hs` program,
- which is called with the `js-import-stories` sub-command,
- and `js-stories` to generate (parts of) `render-components.js`.


## Notes

There is also a custom Revealjs template that (should) match the design-system.

The `docs/` directory is used because this repository can then use the GitHub
Pages feature from the `master` branch.

There is still old examples in the `docs/` directory.


## xelatex PDF example

In the `xelatex/` directory, run:

```
$ nix-build
```

The resulting PDF file is then `result/example.pdf`.


## Elsewhere

- The [IBM design system](https://www.carbondesignsystem.com/) is open source.


## Acknowledgement


- The PDF brochure was designed using Figma by Marina Rizo:
  - Email address: rizo.marina@gmail.com
  - Dribbble: [rinxols](https://dribbble.com/rinxols)
- The LaTeX implementation of the brochure design was done by Vel from
  [LaTeXTypesetting.com](latextypesetting.com).
