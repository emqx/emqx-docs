#!/bin/bash

set -euo pipefail

EDITION="${1:-}"

case $EDITION in
    ce|com*)
        PREFIX='v'
        ;;
    ee|ent*)
        PREFIX='e'
        ;;
    *)
        echo "Usage: $0 ce|ee # ce for EMQX, ee for EMQX Enterprise"
        exit 1
esac

branch="$(git branch | grep -E "^\*" | tr -d "* ")"

case $branch in
    'release-4.3')
        VSN='4.3'
        ;;
    'release-4.4')
        VSN='4.4'
        ;;
    *)
        echo "can not cut release on branch $branch"
        exit 1
esac

TODAY="$(date +%Y%m%d)"

TAG="${PREFIX}${VSN}-${TODAY}"
git tag -f "$TAG"

echo "$TAG"
