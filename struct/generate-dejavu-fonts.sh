#! /usr/bin/env nix-shell
#! nix-shell -i bash -p fontforge

# Generate woff files for the DejaVu Sans Mono font.
#
# This is similar to
# https://github.com/maxwell-k/dejavu-sans-mono-web-font
#
# The resulting woff and woff2 files are committed in static/fonts.

mkdir tmp
cd tmp

wget https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.tar.bz2
tar -xvjf dejavu-fonts-ttf-2.37.tar.bz2

echo 'Open($1)' > convert.pe
echo 'Generate($1:r + ".woff")' >> convert.pe
echo 'Generate($1:r + ".woff2")' >> convert.pe

cp dejavu-fonts-ttf-2.37/ttf/DejaVuSansMono*.ttf .

for i in *.ttf ; do
  fontforge -script convert.pe "$i"
done
