#! /usr/bin/env bash

# Create a fresh generated/ directory with both normal and pretty-printed HTML
# versions of the available components.

set -e

rm -rf generated/
mkdir -p generated/{min,pretty,static}

nix-shell --run "runghc -XDeriveAnyClass -XDeriveGeneric -XDerivingStrategies -XNoImplicitPrelude -XOverloadedStrings -XRecordWildCards -XTypeApplications -XTypeOperators bin/hypered-design.hs generate-guide"

cp -r static/* generated/static/
