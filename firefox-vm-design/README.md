A version of ../../firefox-vm made to create screenshots of Struct pages.

This really should be part of this repository although I need to check if using
its nixpkgs still results in the exact same rendering.

The goal is to create images of the Struct pages so it's possible to refactor
the code and make sure it still looks the same.

# Usage

Build all the screenshots:

```
$ nix-build vm-test.nix
```

Once done, I compare the `result/` directory with a previous copy:

```
$ diff -u -r 2025-04-01/ result/
Binary files 2025-04-01/motherboard-motherboard-document.png and result/motherboard-motherboard-document.png differ
...
```

One way to see the differences is to redirect the above output to a file, and
edit that file to look like:

```
feh 2025-04-01/motherboard-motherboard-document.png result/motherboard-motherboard-document.png
...
```

Then execute that file: is displays each pair of files, making it easy to see
the differences. Quitting `feh` (by typing the letter `q`) will execute the
next line, allowing the see the next pair.
