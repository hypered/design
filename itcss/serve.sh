#! /usr/bin/env bash

# If we were using node_modules:
# ./node_modules/gulp/bin/gulp.js

# But we use a Nix shell -provided node_modules. So:
nix-shell --run 'gulp'
