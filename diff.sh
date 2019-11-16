#! /usr/bin/env bash

# Check that components rendered with either React or Haskell are the same.

set -e

for i in \
  nav \
  footer \
  ; \
do
  echo $i
  nix-shell --run "runghc bin/hypered-guide.hs $i" | \
    nix-shell -p nodejs --run 'node render-components pretty' > a
  nix-shell -p nodejs --run "node render-components $i" > b
  diff -u a b
done
