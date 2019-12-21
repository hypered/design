#! /usr/bin/env bash

# Replace links to .md files by links to .html files. .md links are useful
# within Markdown files. e.g. when rendered by GitHub, but should be replaced
# by their .html counterparts when the target has been converted to .html.

# Path to the directory containing .html files.

# TODO Add subdirectories to test-site.

SITE="$1"

# Find .md links. TODO Use a HTML parser instead, to replace actual links
# instead of every occurence.
grep 'href="[a-z-]*\.md"' -Ir "$SITE" --include '*.html' -ho \
  | sort -u \
  | sed 's/href="\(.*\)"/\1/' \
  > md-links.txt

# Find .html files. Rename their extension so they can be matched against the
# other list.
find "$SITE" -type f -name '*.html' \
  | sed -e "s@^$SITE\/@^@" \
        -e "s/.html$/.md$/" \
  > html-files.txt

# Keep only links with corresponding .html files, and turn then into a sed
# replace script (allow / to appear in links).
grep -f html-files.txt md-links.txt \
  | sed 's@\(.*\).md@s|href="\1.md"|href="\1.html"|@' \
  > md-links-sed.txt

# Run the sed script over the .html files
find "$SITE" -type f -name '*.html' -exec \
  sed -i -f md-links-sed.txt {} \;

rm md-links.txt html-files.txt md-links-sed.txt
