#! /usr/bin/env bash

# If we were using node_modules:
# ../itcss/node_modules/gulp/bin/gulp.js build --gulpfile ../itcss/gulpfile.js --cwd .

# But we use a Nix shell -provided node_modules. So:
nix-shell ../itcss/shell.nix --run 'gulp build --gulpfile ../itcss/gulpfile.js --cwd .'
