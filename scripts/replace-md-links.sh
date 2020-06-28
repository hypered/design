#! /usr/bin/env bash

# Replace links to .md files by links to .html files. .md links are useful
# within Markdown files. e.g. when rendered by GitHub, but should be replaced
# by their .html counterparts when the target has been converted to .html.

# Path to the directory containing .html files.
SITE="$1"

function replace_link
{
  # Path to the .html file.
  FILE="$1"

  # Find .md links. TODO Use a HTML parser instead, to replace actual links
  # instead of every occurence.
  grep 'href="[a-zA-Z0-9/\.-]*\.md"' "$FILE" -ho \
    | sort -u \
    | sed 's@href="\(.*\)\.md"@\1.html@' \
    > md-links.txt

  # Keep only links with existing .html files.
  while read p ; do
    realpath \
      --quiet \
      --canonicalize-existing \
      --relative-to "$(dirname "$FILE")" \
      "$(dirname "$FILE")/$p" > /dev/null
    exit_code=$?
    if [[ $exit_code -eq 0 ]] ; then
      echo "$p"
    fi
  done < md-links.txt | sort -u > md-links-existing.txt

  # Turn those links into a sed replace script (allow / to appear in links).
  cat md-links-existing.txt \
    | sed 's@\(.*\).html@s|href="\1.md"|href="\1.html"|@' \
    > md-links-sed.txt

  # Run the sed script over the .html file.
  sed -i -f md-links-sed.txt "$FILE"
}

# Run the function over the .html files.
export -f replace_link
find "$SITE" -type f -name '*.html' -exec \
  bash -c 'replace_link "$0"' {} \;
