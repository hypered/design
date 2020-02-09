---
title: Pandoc template
nav:
- name: noteed.com
  href: "#"
- name: blog
  href: "#"
- name: not-os
  href: "#"
footer: © Hypered SPRL, 2019-2020.
---

This is an example Markdown file to test the generated templates of the
[Hypered design system](https://github.com/hypered/design-system). In addition
of the templates themselves, there are also Pandoc Lua filters to help format
the final HTML.


## Reference

This page should match this [Storybook
example](../storybook/iframe.html?id=layouts--blog-post).


## Building

This page can be built with the following command:

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
