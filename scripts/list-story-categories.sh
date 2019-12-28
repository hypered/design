#! /usr/bin/env bash

# List the .stories.js filenames. This correpsonds to the Storybook hierarchy
# display on the left in the component explorer.

find components/ -name '*.stories.js' -exec basename {} \; \
  | grep -o '^[^.]\+' \
  | sort
