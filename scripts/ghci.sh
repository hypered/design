#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -ibin/ \
  -isrc/ \
  -XNoImplicitPrelude \
  -XTypeApplications \
  bin/hypered-design.hs
