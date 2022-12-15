#! /usr/bin/env nix-shell
#! nix-shell -i bash

set -e

pdflatex template
biber template
makeglossaries template
pdflatex template
pdflatex template
