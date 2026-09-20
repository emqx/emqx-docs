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
  1. Starts the container with EMQX_dashboard__i18n_lang set and a generated
     Dashboard admin password.
  2. Waits 30s for the swagger endpoint to come up.
  3. Logs in as admin and fetches /api-docs/swagger.json with the bearer token.
     Starting from EMQX 6.3.0, an unauthenticated request gets HTTP 401 and a
     stub spec that lists only /login and /status.
  4. Fails if the spec has no release version or too few paths.
  5. Strips deprecated operations, rewrites 172.17.0.* host references to
     127.0.0.1, and writes redocly/ee-<lang>.json.
  6. Dumps container logs and removes the container.

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

# Use a generated password so the login works under any security profile.
# The hardened profile rejects the built-in default password.
DASHBOARD_PASSWORD="$(head -c 32 /dev/urandom | base64 | tr -dc 'A-Za-z0-9' | head -c 20)"

# A complete spec has hundreds of paths. The unauthenticated stub has two.
MIN_PATHS=50

CID=""
cleanup() {
  if [ -n "${CID}" ]; then
    docker rm -f "${CID}" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

EMQX_API_PORT=18083
for lang in en zh; do
  CID=$(docker run -d -p $EMQX_API_PORT:18083 \
    -e EMQX_dashboard__i18n_lang=${lang} \
    -e EMQX_DASHBOARD__DEFAULT_PASSWORD="${DASHBOARD_PASSWORD}" \
    "${DOCKER_IMAGE}")
  # it takes some time for swagger to fully load
  sleep 30
  SWAGGER_INPUT="/tmp/swagger-ee-${lang}.json"
  REDOCLY_TARGET="redocly/ee-${lang}.json"
  TOKEN=$(curl -fsS -X POST "http://127.0.0.1:$EMQX_API_PORT/api/v5/login" \
    -H 'content-type: application/json' \
    -d "{\"username\":\"admin\",\"password\":\"${DASHBOARD_PASSWORD}\"}" | jq -r .token)
  curl -fsS -H "Authorization: Bearer ${TOKEN}" -o "${SWAGGER_INPUT}" \
    "http://127.0.0.1:$EMQX_API_PORT/api-docs/swagger.json"
  if ! jq -e --argjson min "${MIN_PATHS}" \
      '(.info.version != "unknown") and ((.paths | length) >= $min)' \
      "${SWAGGER_INPUT}" >/dev/null; then
    echo "ERROR: ${SWAGGER_INPUT} is not a complete spec" \
      "(info.version=$(jq -r .info.version "${SWAGGER_INPUT}")," \
      "paths=$(jq '.paths | length' "${SWAGGER_INPUT}"))" >&2
    exit 1
  fi
  jq -S --indent 2 'del(.paths[] | .[] | select(.deprecated == true))' > "${REDOCLY_TARGET}" < "${SWAGGER_INPUT}"
  # replace docker ip 172.17.0.* with 127.0.0.1
  sed -i 's/172\.17\.0\.\([0-9]\{1,3\}\)/127.0.0.1/g' "${REDOCLY_TARGET}"
  docker logs "$CID"
  docker rm -f "$CID"
  CID=""

  EMQX_API_PORT=$((EMQX_API_PORT + 1))
done
