#!/usr/bin/env bash

set -euo pipefail

PROFILE=emqx-enterprise

usage() {
  cat <<EOF
Usage: $(basename "$0") <emqx-version> [docker-image]

Regenerate the redocly OpenAPI sources (redocly/ee-en.json, redocly/ee-zh.json)
from a running EMQX Enterprise container, and bump current-version.env to match.

Arguments:
  <emqx-version>   EMQX Enterprise version tag, e.g. 5.10.4 or 5.10.4-rc.4.
                   Used to set EE_VERSION / EE_MINOR_VERSION in current-version.env
                   and, unless overridden, to pick the docker image tag.
  [docker-image]   Optional full docker image reference.
                   Default: emqx/${PROFILE}:<emqx-version>

What it does, per language (en, zh):
  1. Starts the container with EMQX_dashboard__i18n_lang set.
  2. Waits 30s for the swagger endpoint to come up.
  3. Fetches /api-docs/swagger.json, strips deprecated operations,
     rewrites 172.17.0.* host references to 127.0.0.1, and writes
     redocly/ee-<lang>.json.
  4. Dumps container logs and removes the container.

Requirements: docker, curl, jq, sed. Run from the repo root.

Examples:
  $(basename "$0") 5.10.4
  $(basename "$0") 5.10.4-rc.4 emqx/emqx-enterprise:5.10.4-rc.4
EOF
}

case "${1:-}" in
  -h|--help|"")
    usage
    [ -z "${1:-}" ] && exit 1 || exit 0
    ;;
esac

EMQX_VERSION=${1}
DOCKER_IMAGE=${2:-"emqx/${PROFILE}:${EMQX_VERSION}"}

VERSION=$(echo "${EMQX_VERSION}" | cut -d '-' -f 1)
MINOR_VERSION=$(echo "${EMQX_VERSION}" | cut -d '.' -f 1-2)
sed -i "s/EE_VERSION=.*/EE_VERSION=${VERSION}/" ./current-version.env
sed -i "s/EE_MINOR_VERSION=.*/EE_MINOR_VERSION=${MINOR_VERSION}/" ./current-version.env

EMQX_API_PORT=18083
for lang in en zh; do
  CID=$(docker run -d -p $EMQX_API_PORT:18083 -e EMQX_dashboard__i18n_lang=${lang} "${DOCKER_IMAGE}")
  # it takes some time for swagger to fully load
  sleep 30
  SWAGGER_INPUT="/tmp/swagger-ee-${lang}.json"
  REDOCLY_TARGET="redocly/ee-${lang}.json"
  curl -o "${SWAGGER_INPUT}" http://127.0.0.1:$EMQX_API_PORT/api-docs/swagger.json
  jq -S --indent 2 'del(.paths[] | .[] | select(.deprecated == true))' > "${REDOCLY_TARGET}" < "${SWAGGER_INPUT}"
  # replace docker ip 172.17.0.* with 127.0.0.1
  sed -i 's/172\.17\.0\.\([0-9]\{1,3\}\)/127.0.0.1/g' "${REDOCLY_TARGET}"
  docker logs "$CID"
  docker rm -f "$CID"

  EMQX_API_PORT=$((EMQX_API_PORT + 1))
done
