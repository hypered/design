#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -ibin/ \
  -isrc/ \
  -XDeriveAnyClass \
  -XDeriveGeneric \
  -XDerivingStrategies \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  bin/hypered-design.hs
