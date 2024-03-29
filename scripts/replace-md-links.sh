#! /usr/bin/env bash

# Replace links to .md files by links to .html files. .md links are useful
# within Markdown files. e.g. when rendered by GitHub, but should be replaced
# by their .html counterparts when the target has been converted to .html.

# Path to the directory containing .html files.
export SITE="$1"

# Possible prefix to remove. For instance if an about page is within the
# repository at /pages/about.md, but you want to remove '/pages' in the actual
# site (e.g. when the build happens within /pages).
PREFIX_TO_REMOVE="${2:-}"

# If true, remove the .html extension.
REMOVE_SUFFIX="${3:-}"

function replace_link
{
  # Path to the .html file.
  FILE="$1"
  PREFIX_TO_REMOVE="${2:-}"
  REMOVE_SUFFIX="${3:-}"

  # Find .md links. TODO Use a HTML parser instead, to replace actual links
  # instead of every occurence.
  grep 'href="[a-zA-Z0-9/\.-]*\.md"' "$FILE" -ho \
    | sort -u \
    | sed 's@href="'$PREFIX_TO_REMOVE'\(.*\)\.md"@\1.html@' \
    > md-links.txt

  # Keep only links with existing .html files.
  while read p ; do
    if [[ "$p" =~ ^/ ]] ; then
      q="$SITE/$p"
    else
      q="$(dirname "$FILE")/$p"
    fi
    realpath \
      --quiet \
      --canonicalize-existing \
      --relative-to "$(dirname "$FILE")" \
      "$q" > /dev/null
    exit_code=$?
    if [[ $exit_code -eq 0 ]] ; then
      echo "$p"
    fi
  done < md-links.txt | sort -u > md-links-existing.txt

  # Turn those links into a sed replace script (allow / to appear in links).
  if [[ -z "$REMOVE_SUFFIX" ]] ; then
    cat md-links-existing.txt \
      | sed 's@\(.*\).html@s|href="'$PREFIX_TO_REMOVE'\1.md"|href="\1.html"|@' \
      > md-links-sed.txt
  else
    cat md-links-existing.txt \
      | sed 's@\(.*\).html@s|href="'$PREFIX_TO_REMOVE'\1.md"|href="\1"|@' \
      > md-links-sed.txt
  fi

  # Run the sed script over the .html file.
  sed -i -f md-links-sed.txt "$FILE"
}

# Run the function over the .html files.
export -f replace_link
find "$SITE" -type f -name '*.html' -exec \
  bash -c "replace_link \"\$0\" $PREFIX_TO_REMOVE $REMOVE_SUFFIX" {} \;
