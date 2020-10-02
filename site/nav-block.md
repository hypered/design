---
title: Navigation block
nav:
- name: noteed.com
  href: "#"
- name: blog
  href: "#"
- name: not-os
  href: "#"
nav-block:
- title: Design system
  nav:
  - name: Intro
    href: "#"
- title: Components
  nav:
  - name: Footer
    href: "#"
  - name: Navigation
    href: "#"
footer: © Hypered SPRL, 2019-2020.
---

This is an example Markdown file demonstrating the navigation block part of the
design system Pandoc template, visible at the bottom when rendered.


## Building

This page can be built with the following command:

```
$ pandoc \
  --standalone \
  --template pandoc/default.html \
  --lua-filter pandoc/tachyons.lua \
  --output nav-block.html \
  -M prefix="" 
  -M font=inter \
  site/nav-block.md
```
