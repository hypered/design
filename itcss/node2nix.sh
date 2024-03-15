#! /usr/bin/env nix-shell
#! nix-shell -i bash -p node2nix

# We need to re-generate those files whenever `package-lock.json` changes.

mkdir -p nix/node
node2nix -18 \
    --development \
    --input package.json \
    --lock package-lock.json \
    --node-env ./nix/node/node-env.nix \
    --composition ./nix/node/default.nix \
    --output ./nix/node/node-packages.nix
