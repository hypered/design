#! /usr/bin/env bash

# Check that components rendered with either React or Haskell are the same.

set -e

function norm {
  # blaze-html renders hr as <hr> while the react code uses <hr />.
  sed -e 's@<hr class="bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4">@<hr class="bt bb-0 br-0 bl-0 mh0 mt4 pb4 w4" />@'
}

for i in \
  nav \
  footer \
  ; \
do
  echo $i
  nix-shell --run "runghc bin/hypered-guide.hs $i" | \
    nix-shell -p nodejs --run 'node render-components pretty' | \
    norm > a
  nix-shell -p nodejs --run "node render-components $i" > b
  diff -u a b
done
