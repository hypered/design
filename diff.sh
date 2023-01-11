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
  banner--green \
  banner--red \
  banner--yellow \
  blockquote--default \
  blockquote--pull-quote-example \
  blockquote--with-optional-pull-quote-example \
  button--primary \
  button--primary-large \
  button--primary-disabled \
  button--secondary \
  button--secondary-large \
  button--secondary-disabled \
  button--full-width \
  buttonlink--primary \
  buttonlink--primary-large \
  buttonlink--primary-disabled \
  buttonlink--secondary \
  buttonlink--secondary-large \
  buttonlink--secondary-disabled \
  buttonlink--full-width \
  footer \
  nav \
  table--default \
  table--compact \
  table--with-column-divider \
  table--with-column-divider-compact \
  ; \
do
  echo $i
  hypered-design $i | \
    node render-components pretty | \
    norm > a
  node render-components $i > b
  diff -u a b
done

rm a b
