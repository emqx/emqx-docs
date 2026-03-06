#!/usr/bin/env bash

set -euo pipefail

DEV_CHANGES_DIR=$1
EMQX_VERSION=$2

[ -z "${DEBUG:-}" ] || set -x

process_changes() {
  local source_edition=$1
  local target_edition=${2:-source_edition}

  [ ! -d $DEV_CHANGES_DIR/$source_edition ] && return

  local major_minor=$(echo $EMQX_VERSION | cut -d. -f1,2)
  local major=$(echo $EMQX_VERSION | cut -d. -f1)

  # Add a section for the new version if it doesn't exist along with the Enhancements and Bug Fixes sections
  if ! grep -q "^## $EMQX_VERSION" en_US/changes/changes-$target_edition-v$major.md; then
    sed -i "3i ## $EMQX_VERSION\n" en_US/changes/changes-$target_edition-v$major.md
    sed -i "5i ### Enhancements\n" en_US/changes/changes-$target_edition-v$major.md
    sed -i "7i ### Bug Fixes\n" en_US/changes/changes-$target_edition-v$major.md
  fi

  # Get the line number of the Enhancements section
  enhancements_ln=$(grep -n '^### Enhancements' "en_US/changes/changes-${target_edition}-v${major}.md" | head -n 1 | cut -d: -f1)
  # Increment the line number to leave a blank line before the first enhancement
  enhancements_ln=$((enhancements_ln + 1))

  shopt -s nullglob

  for f in $DEV_CHANGES_DIR/$source_edition/fix-*.md $DEV_CHANGES_DIR/$source_edition/feat-*.md; do
    pr_num="$(echo "${f}" | sed -E 's/.*-([0-9]+)\.[a-z]+\.md$/\1/')"
    if ! grep -q "^- \[#$pr_num\]" en_US/changes/changes-$target_edition-v$major.md; then
      {
        echo "- [#${pr_num}](https://github.com/emqx/emqx/pull/${pr_num}) $(head -n 1 "$f")"
        # indent the content
        tail -n +2 "$f" | sed '/^$/!s/^/  /'
        echo ""
      } > /tmp/$pr_num.md

      if [[ "$f" =~ ^$DEV_CHANGES_DIR/$source_edition/feat-.*\.md ]]; then
        sed -i "${enhancements_ln}r /tmp/$pr_num.md" en_US/changes/changes-$target_edition-v$major.md
      elif [[ "$f" =~ ^$DEV_CHANGES_DIR/$source_edition/fix-.*\.md ]]; then
        # Get the line number of the Bug Fixes section
        bugfixes_ln=$(grep -n 'Bug Fixes' en_US/changes/changes-$target_edition-v$major.md | head -n 1 | cut -d: -f1)
        # Increment the line number to leave a blank line before the first bug fix
        bugfixes_ln=$((bugfixes_ln + 1))
        sed -i "${bugfixes_ln}r /tmp/$pr_num.md" en_US/changes/changes-$target_edition-v$major.md
      fi
    fi
  done

  # count number of files matching $DEV_CHANGES_DIR/$source_edition/breaking-*.md pattern
  num_files=$(find $DEV_CHANGES_DIR/$source_edition -type f -name 'breaking-*.md' | wc -l)
  # exit the function if no breaking changes files are found
  [ $num_files -eq 0 ] && return

  breaking_changes="en_US/changes/breaking-changes-$major_minor.md"

  # create en_US/changes/breaking-changes-$source_edition-$major_minor.md if it does not exist
  if [ ! -f "${breaking_changes}" ]; then
    echo "# Incompatible Changes in EMQX $major_minor" > "${breaking_changes}"
    echo "" >> "${breaking_changes}"
    echo "" >> "${breaking_changes}"
  fi

  # Add a section for the new version in breaking-changes if it doesn't exist
  if ! grep -q "^## $EMQX_VERSION" "${breaking_changes}"; then
    version=$( [[ $target_edition = ce ]] && echo "v$EMQX_VERSION" || echo "e$EMQX_VERSION" )
    sed -i "3i ## $version\n" "${breaking_changes}"
  fi

  for f in $DEV_CHANGES_DIR/$source_edition/breaking-*.md; do
    pr_num="$(echo "${f}" | sed -E 's/.*-([0-9]+)\.[a-z]+\.md$/\1/')"
    if ! grep -q "^- \[#$pr_num\]" "${breaking_changes}"; then
      {
        echo "- [#${pr_num}](https://github.com/emqx/emqx/pull/${pr_num}) $(head -n 1 "$f")"
        # indent the content
        tail -n +2 "$f" | sed '/^$/!s/^/  /'
        echo ""
      } > /tmp/$pr_num.md

      sed -i "4r /tmp/$pr_num.md" "${breaking_changes}"
    fi
  done
}

process_changes ce ee
process_changes ee ee
