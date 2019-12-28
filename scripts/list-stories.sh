#! /usr/bin/env bash

# List exported component examples from .stories.js files.

# The assumption is that each component example is listed as
#
#   export const Green = ...

# - Grep for each export line...
# - Sort them...
# - Each line looks like
#    components/Banner/Banner.stories.js:export const Green = ...
# - Create match groups for the category name, then the component name
#   and lowercase the first match (\L\1\E), insert a dash before the second
#   match...
# - Replace uppercase character by a dash and the lowercase character (-\L\1).
grep '^export const' -Ir components/ --include '*.stories.js' \
  | sort \
  | sed -s 's@components/[^/]\+/\([^.]\+\).stories.js:export const \([^ ]\+\) .*@\L\1\E-\2@' \
  | sed -s 's@\([A-Z]\)@-\L\1@g'
