# Compare two builds of the brochure page by page, e.g. the one built with
# nixpkgs-brochure (the reference) and one built with a newer nixpkgs.
#
#   nix-build -A brochure-comparison
#   cat result/summary.txt
{ nixpkgs, reference, candidate }:

nixpkgs.runCommand "brochure-comparison" {
  nativeBuildInputs = [ nixpkgs.poppler-utils nixpkgs.imagemagick ];
} ''
  mkdir -p $out pages
  ref=${reference}/example-full.pdf
  new=${candidate}/example-full.pdf

  {
    echo "reference: $ref"
    pdfinfo "$ref" | grep -E '^(Producer|Pages)'
    echo "candidate: $new"
    pdfinfo "$new" | grep -E '^(Producer|Pages)'
    echo
  } > $out/summary.txt

  pdftoppm -r 100 -png "$ref" pages/ref
  pdftoppm -r 100 -png "$new" pages/new

  for r in pages/ref-*.png; do
    n=''${r#pages/ref-}
    n=''${n%.png}
    c=pages/new-$n.png
    if [ ! -e "$c" ]; then
      echo "page $n: missing from candidate" >> $out/summary.txt
      continue
    fi
    px=$(magick compare -metric AE -fuzz 10% "$r" "$c" $out/$n-diff.png 2>&1 || true)
    px=''${px%% *}
    px=''${px%%.*}
    # Below ~0.01% of a page at 100 dpi, differences are anti-aliasing?
    if [ "$px" -lt 100 ]; then
      rm $out/$n-diff.png
      echo "page $n: same ($px differing pixels, anti-aliasing)" >> $out/summary.txt
    else
      magick "$r" "$c" +append $out/$n-side-by-side.png
      echo "page $n: $px differing pixels" >> $out/summary.txt
    fi
  done

  pdftotext -layout "$ref" ref.txt
  pdftotext -layout "$new" new.txt
  diff ref.txt new.txt > $out/text.diff || true
  echo >> $out/summary.txt
  echo "text.diff: $(grep -c '^[<>]' $out/text.diff || true) differing lines" >> $out/summary.txt
''
