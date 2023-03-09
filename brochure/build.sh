#! /usr/bin/env nix-shell
#! nix-shell -i bash

set -e

TEX_FILENAME="example-full"

pdflatex "${TEX_FILENAME}"
biber "${TEX_FILENAME}"
makeglossaries "${TEX_FILENAME}"
pdflatex "${TEX_FILENAME}"
pdflatex "${TEX_FILENAME}"
