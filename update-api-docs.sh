#!/usr/bin/env bash

set -euo pipefail

PROFILE=emqx-enterprise

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
  jq --indent 2 'del(.paths[] | .[] | select(.deprecated == true))' > "${REDOCLY_TARGET}" < "${SWAGGER_INPUT}"
  # replace docker ip 172.17.0.* with 127.0.0.1
  sed -i 's/172\.17\.0\.\([0-9]\{1,3\}\)/127.0.0.1/g' "${REDOCLY_TARGET}"
  docker logs "$CID"
  docker rm -f "$CID"

  EMQX_API_PORT=$((EMQX_API_PORT + 1))
done

