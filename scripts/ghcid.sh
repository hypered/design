#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghcid --warnings --command scripts/ghci.sh
