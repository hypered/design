#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -ibin/ \
  -isrc/ \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  bin/hypered-design.hs
