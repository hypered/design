---
title: Pandoc template (Struct)
nav:
- name: noteed.com
  href: "#"
- name: blog
  href: "#"
- name: not-os
  href: "#"
footer: © Hypered SPRL, 2019-2024.
---

This is an example Markdown file to test the generated templates of the
[Hypered design system](https://github.com/hypered/design).


## Building

This page can be built with the following command:

```
$ pandoc \
  --standalone \
  --template pandoc/struct.html \
  -M prefix="" \
  pandoc/struct.md
```
