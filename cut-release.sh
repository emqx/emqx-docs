#!/bin/bash

# starting from 5.9, always tag a new release using 'v' prefix

set -euo pipefail

VSN="${1:-}"
if [ -z "${VSN}" ]; then
    echo "usage: $0 VERSION"
    echo "e.g. $0 v5.9"
fi

TODAY="$(date +%Y%m%d)"

TAG="${VSN}-${TODAY}"
git tag -f "$TAG"

echo "$TAG"
