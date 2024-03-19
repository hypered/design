#! /usr/bin/env nix-shell
#! nix-shell -i bash

set -e

# Actually example-large-table can be built with just one
# call to pdflatex.
for TEX_FILENAME in \
  example-font \
  example-full \
  example-large-table \
  ; do
  pdflatex "${TEX_FILENAME}"
  biber "${TEX_FILENAME}"
  makeglossaries "${TEX_FILENAME}"
  pdflatex "${TEX_FILENAME}"
  pdflatex "${TEX_FILENAME}"
done

