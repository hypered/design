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
- `blaze-html` (Haskell). A script [`diff.sh`](diff.sh) ensures the generated
  code is similar to the Next.js implementation.
- static HTML snippets
- Pandoc templates
- Scripts to modify Haddock results
- XML + XSLT stylesheet (see [README](/xslt/README.md))
- EPUB (see [README](/epub/README.md))
- ...

The original site built with Next.js is visible at
[js.hypered.design](https://js.hypered.design). It also contains a
[component explorer](https://js.hypered.design/storybook/) based on
[Storybook](https://storybook.js.org/).

Other build results from this repository are visible at
[hypered./design](https://hypered.design). In particular, the same components,
but rendered using `blaze-html`, are visible at
[hypered./design/hs/](https://hypered.design/hs/).

Note: there are two sites: the main one is built using Nix and hosted on a VM;
the second one is hosted on GitHub Pages out of the `docs/` directory (which is
is populated manually).


# Static website

The static website at
[hypered.github.io/design](https://hypered.github.io/design) contains content
generated (or copied as-is) from different parts of this repository. Or,
conversely, the following directories (or programs) generate parts of the
static website:

- `bin/hypered-guide.hs` (generates [design/hs](https://hypered.github.io/design/hs/))
- `components/` (processed with Next.js and Storybook)
  - [components](https://hypered.github.io/design/storybook/)
- `docs/` (i.e. as-is HTML pages):
  - [sprint-2](https://hypered.github.io/design/sprint-2.html)
  - [blog](https://hypered.github.io/design/blog/)
  - [blog post](https://hypered.github.io/design/blog/starting-with-nixops-1.html)
  - [blog post](https://hypered.github.io/design/blog/starting-with-nixops-2.html)
  - [not-os](https://hypered.github.io/design/projects/not-os/)
  - [waveguide](https://hypered.github.io/design/projects/waveguide.html)
  - [nubs-bash](https://hypered.github.io/design/nubs-bash/)
  - [nubs-bash/hello](https://hypered.github.io/design/nubs-bash/hello/)
- `pages/` (processed with Next.js)
  - [/](https://hypered.github.io/design/) (the landing page)
  - [components](https://hypered.github.io/design/components/) (components preview)
- `site/` (processed with `site/default.nix`):
  - [nav-block](https://hypered.github.io/design/nav-block.html)
  - [nix](https://hypered.github.io/design/nix.html)
  - [Haddock](https://hypered.github.io/design/haddock/) (copied and modified from the binary)


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
$ npx browserslist@latest --update-db  # optional
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

To build and serve the Haskell-based site locally:

```
$ scripts/build-haskell-guide.sh
$ nix-shell -p busybox --run "httpd -f -p 9000 -h generated/"
```

The visit e.g. [127.0.0.1:9000/pretty/](http://127.0.0.1:9000/pretty/).


## Rendering React components from the command-line

A helper Node.js script is provided to render components from the command-line:

```
$ node render-components footer
<footer>
  <hr class="bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4" />
  <p class="inline-flex lh-copy">© Hypered, 2019.</p>
</footer>
```


## Haskell code

There are two parts to the Haskell code, found in `bin/` and `src/`.

One is the `Hypered.Html` module, that is meant to be re-used across Hypered
projects to benefit from the Haskell implementation of the design system.

The other is `Hypered.Design` which is the code behind the `hypered-design`
command-line tool. That tool can generate the design system website (i.e. the
"guide") at `hypered.design`, HTML templates for Pandoc, or show individual
component code (that can be compared with the `./diff.sh` script against the
Next.js implementation).

The `hypered-design` program has also a `serve` subcommand that is uses as a
backend on `hypered.design`. It currently serves a echo handler for the login
form and is meant to evolve to serve e.g. HTML fragments that can be used with
something like htmx.

It can also be used locally to work on a page that is automatically refreshed
in a browser.


### Comparing Haskell and Node outputs

In addition of component names, the `render-components` script can process its
stdin with 'pretty' to normalize some input HTML.

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

A Pandoc template can be generated with the `bin/hypered-design.hs` script.
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

[Here](https://hypered.github.io/design/hs/example--template.html) is
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
[Here](https://hypered.github.io/design/hs/example--template-ibm-plex.html)
with a different font.


## Markdown pages

Part of the static site are Markdown pages rendered to HTML using Pandoc:

```
$ nix-build -A site
```


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

- `scripts/create-stories-hs` is used to generate `Hypered/Design/Stories.hs`,
- which is imported in the `bin/hypered-guide.hs` program,
- which is called with the `js-import-stories` sub-command,
- and `js-stories` to generate (parts of) `render-components.js`.


## Notes

There is also a custom Revealjs template that (should) match the design system.

The `docs/` directory is used because this repository can then use the GitHub
Pages feature from the `main` branch.


## xelatex PDF example

In the `xelatex/` directory, run:

```
$ nix-build
```

The resulting PDF file is then `result/example.pdf`.


## Screenshots

To create a higher resolution screenshot, in Firefox:

```
> :screenshot --dpr 4 --fullpage
```

I don't see a way to specify the viewport dimensions, which may be necessary
for e.g. `height: 100vh`. But I can make the developer tools a separate window
(on a different workspace), put the page in fullscreen, and take the screenshot
and it works.


## Elsewhere

- The [IBM design system](https://www.carbondesignsystem.com/) is open source.


## Acknowledgement

- The initial Next.js + Storybook -based work was done by
  [@andyngo](https://twitter.com/andyngo).
- The PDF brochure was designed using Figma by Marina Rizo:
  - Email address: rizo.marina@gmail.com
  - Dribbble: [rinxols](https://dribbble.com/rinxols)
- The LaTeX implementation of the brochure design was done by Vel from
  [LaTeXTypesetting.com](https://latextypesetting.com).
- The [GOV.UK design system](https://github.com/alphagov/govuk-design-system)
  inspires parts of this work (e.g. the two thirds / one third columns).
