---
title: Hypered design system
footer: © Hypered, 2019-2023.
---


## Introduction

The Hypered design system is a collection of components, templates, and other
bits of HTML and CSS to create a unified user experience across Hypered
projects. It also contains non-web implementations, such as LaTeX templates.

See the README file in the GitHub
[repository](https://github.com/hypered/design) for additional information.

## Content

The initial design work was done in Next.js. It made it possible to create
re-usable snippets of HTML show-cased in Storybook. Two pages to introduce the
design system were also created, with the addition of an error page. The CSS
part was done using Tachyons.

- [Landing page](landing/) - An initial landing page for the design system
  itself.
- [Components page](components/) - A similar page to the landing page, to give
  an overview of the available components.
- [404 page](404/) - An error page using CSS animations.
- [Components (Storybook)](storybook/) - A Storybook-based page to explore all
  the components.

There is an attempt at creating an implementation of the design system using
the ITCSS approach. Only a single page exists for now.

- [ITCSS example page](static-binaries.html) - This page should match an
  [existing page](https://noteed.com/notes/static-binaries.html).

The main usage of the design system is within applications written in Haskell.
Examples generated with the Haskell code are also available:

- [Haskell examples](hs/) - Everything that exist in the original
  implementation also exist here.

## See also

Another attempt at creating a Pandoc template is visible at
[alt.hypered.design](https://alt.hypered.design).
