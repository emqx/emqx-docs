#!/bin/bash

set -euo pipefail

EDITION="${1:-}"

case $EDITION in
    ce|open*)
        PREFIX='v'
        ;;
    ee|ent*)
        PREFIX='e'
        ;;
    *)
        echo "Usage: $0 ce|ee # ce for opensource edition, ee for enterprise"
        exit 1
esac

branch="$(git branch | grep -E "^\*" | tr -d "* ")"

case $branch in
    'release-5.0')
        VSN='5.0'
        ;;
    *)
        echo "can not cut release on branch $branch"
        exit 1
esac

TODAY="$(date +%Y%m%d)"

TAG="${PREFIX}${VSN}-${TODAY}"
git tag -f "$TAG"

echo "$TAG"
