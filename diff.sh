#! /usr/bin/env bash

# Check that components rendered with either React or Haskell are the same.
# Run this script within the `nix-shell default.nix -A shell` shell.

set -e

function norm {
  # blaze-html renders hr as <hr> while the react code uses <hr />.
  sed -e 's@<hr class="\(.*\)">@<hr class="\1" />@'
}

for i in \
  a--blue \
  a--black \
  nav \
  footer \
  ; \
do
  echo $i
  hypered-design $i | \
    node render-components pretty | \
    norm > a
  node render-components $i > b
  diff -u a b
done
