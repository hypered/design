Attempt at generating graphs with D3.js.

A `package.json` file is provided to install the required dependencies. Once
the dependencies are installed, it is possible to build everything with the
`build.sh` script.

```
$ nix-shell -p nodejs
$ npm install
$ ./build.sh
```

The result is in the `dist/` directory.

Each graph should be a JS file in the `graphs/` directory, exporting a `draw()`
function.
