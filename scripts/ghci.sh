#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# We use a ghci.conf file instead of loading directly bin/refli.hs. This lets
# us add additional modules for convenience when trying expressions inside
# GHCi.

ghc --interactive \
  -ibin/ \
  -isrc/ \
  -itests/ \
  -XDeriveAnyClass \
  -XDeriveGeneric \
  -XDerivingStrategies \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XTypeApplications \
  -XTypeOperators \
  -Wall \
  -ghci-script scripts/ghci.conf
