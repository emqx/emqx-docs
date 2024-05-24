#!/bin/bash
set -euo pipefail

## this script is to run a docker container to
## 1. render the markdown files for vuepress
## 2. serve the rendered HTML pages in a vuepress site
##
## It takes one argument as the listner port number the
## port number which defaults to 8080

PRODUCT="${1:-ce}" # ce or ee
PORT="${2:-8080}"

THIS_DIR="$(cd "$(dirname "$(readlink "$0" || echo "$0")")"; pwd -P)"

docker rm emqx-doc-preview > /dev/null 2>&1 || true

if [ "$PRODUCT" = "ce" ]; then
    python3 "$THIS_DIR/gen.py" ce > directory.json
    docker run -p ${PORT}:8080 -it --name emqx-doc-preview \
        -v "$THIS_DIR"/directory.json:/app/docs/.vitepress/config/directory.json \
        -v "$THIS_DIR"/en_US:/app/docs/en/emqx/latest \
        -v "$THIS_DIR"/zh_CN:/app/docs/zh/emqx/latest \
        -e DOCS_TYPE=emqx \
        -e VERSION=latest \
    ghcr.io/emqx/docs-emqx-com-next:latest
else
    python3 "$THIS_DIR/gen.py" ee > directory_ee.json
    docker run -p ${PORT}:8080 -it --name emqx-doc-preview \
        -v "$THIS_DIR"/directory_ee.json:/app/docs/.vuepress/config/directory.json \
        -v "$THIS_DIR"/en_US:/app/docs/en/enterprise/latest \
        -v "$THIS_DIR"/zh_CN:/app/docs/zh/enterprise/latest \
        -e DOCS_TYPE=enterprise \
        -e VERSION=latest \
    ghcr.io/emqx/docs-emqx-com-next:latest
fi
