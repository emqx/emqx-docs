#!/bin/bash
set -euo pipefail

## this script is to run a docker container to
## 1. render the markdown files for vuepress
## 2. serve the rendered HTML pages in a vuepress site
##
## It takes one argument as the listner port number the
## port number which defaults to 8080

PRODUCT="${1:-ce}" # ce or ee
HOST_PORT="${2:-8080}"
## FIXME: the preview container will print docs link to stdout
## but it is not the host port with `localhost`
## i.e.
## use command `./preview.sh ee 8081`
## but the container printed inner local address:
## ```
##  vitepress v1.0.0-alpha.75
##
##  ➜  Local:   http://localhost:8080/
##  ➜  Network: http://172.17.0.2:8080/
## ```
CONTAINER_NAME="emqx-doc-preview-${PRODUCT}"

THIS_DIR="$(cd "$(dirname "$(readlink "$0" || echo "$0")")"; pwd -P)"

docker rm "$CONTAINER_NAME" > /dev/null 2>&1 || true

if [ "$PRODUCT" = "ce" ]; then
    docker run -p ${HOST_PORT}:8080 -it --name "$CONTAINER_NAME" \
        -v "$THIS_DIR"/directory.json:/app/docs/.vitepress/config/directory.json \
        -v "$THIS_DIR"/en_US:/app/docs/en/emqx/latest \
        -v "$THIS_DIR"/zh_CN:/app/docs/zh/emqx/latest \
        -v "$THIS_DIR"/swagger:/app/docs/.vitepress/public/api \
        -e DOCS_TYPE=emqx \
        -e VERSION=latest \
    ghcr.io/emqx/docs-emqx-com-next:latest
else
    docker run -p ${HOST_PORT}:8080 -it --name "$CONTAINER_NAME" \
        -v "$THIS_DIR"/directory_ee.json:/app/docs/.vitepress/config/directory.json \
        -v "$THIS_DIR"/en_US:/app/docs/en/enterprise/latest \
        -v "$THIS_DIR"/zh_CN:/app/docs/zh/enterprise/latest \
        -e DOCS_TYPE=enterprise \
        -e VERSION=latest \
    ghcr.io/emqx/docs-emqx-com-next:latest
fi
