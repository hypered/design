#! /usr/bin/env bash

# Check that components rendered with either React or Haskell are the same.
# Run this script within the `nix-shell default.nix -A shell` shell.

set -e

function norm {
  # blaze-html renders hr as <hr> while the react code uses <hr />.
  # Same for input.
  sed -e 's@<hr class="\(.*\)">@<hr class="\1" />@' \
    | sed -e 's@<input \(.*\)">@<input \1" />@'
}

for i in \
  a--blue \
  a--black \
  banner--green \
  banner--red \
  banner--yellow \
  blockquote--default \
  blockquote--pull-quote \
  blockquote--with-optional-pull-quote \
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
  checkbox--default \
  checkbox--pill \
  codeblock--default \
  codeblock--with-table \
  codeblock--editable \
  codeblock--textarea \
  codeblock--editable-bottom-button \
  codeblock--editable-toolbar-button \
  codeblock--textarea-bottom-button \
  codeblock--textarea-toolbar-button \
  colour--text \
  colour--background \
  colour--samples \
  container-with-label--default \
  footer \
  nav \
  radio--pill \
  radio--pill-inline \
  radio--checkbox \
  radio--checkbox-inline \
  sidepanel--default \
  sidepanel--usage \
  sidebar--default \
  sidebar--usage \
  table--default \
  table--compact \
  table--with-column-divider \
  table--with-column-divider-compact \
  title--default \
  title--subtitle \
  title--jumbo \
  title--subtitle-jumbo \
  title--usage \
  title--jumbo-usage \
  typography--heading-1 \
  typography--heading-2 \
  typography--heading-3 \
  typography--heading-4 \
  typography--heading-5 \
  typography--heading-6 \
  typography--paragraph \
  typography--usage \
  whitespace--auto-width \
  whitespace--examples \
  whitespace--full-width \
  whitespace--negative-margins \
  ; \
do
  echo $i
  # I don't know why single quotes are escaped differently.
  hypered-design $i > aa
  hypered-design $i | \
    node render-components pretty \
    | norm \
    | sed -e 's@39;@x27;@' \
    > a
  node render-components $i > b
  diff -u a b
done

rm a b
