#! /usr/bin/env nix-shell
#! nix-shell -i bash ../itcss/shell.nix

gulp --gulpfile ../itcss/gulpfile.js --cwd .
