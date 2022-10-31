#!/usr/bin/env bash

## This script requres jq 1.6

set -euo pipefail

SWAGGER_INPUT="${1:-"http://localhost:18083/api-docs/swagger.json"}"

## download swagger from EMQX api-docs
if [ -f "$SWAGGER_INPUT" ]; then
    echo "Using swagger JSON file $SWAGGER_INPUT"
    SWAGGER_INPUT_FILE="$SWAGGER_INPUT"
else
    SWAGGER_INPUT_FILE='/tmp/swagger-filter-stage0.json'
    echo "Downloading swagger JSON from $SWAGGER_INPUT"
    curl "$SWAGGER_INPUT" > "$SWAGGER_INPUT_FILE"
fi

## change the deprecated endpoints to "__DEPRECATED__"
(jq 'walk( if type == "object" then ( if .deprecated == true then "__DEPRECATED__" else . end ) else . end)' > /tmp/swagger-filter-stage1.json)<"$SWAGGER_INPUT_FILE"

## if a path has all methods "__DEPRECATED__", change the path to "__DEPRECATED__"
(jq 'def all_deprecated: to_entries | length > 0 and all(.value == "__DEPRECATED__"); walk ( if type == "object" then ( if all_deprecated then "__DEPRECATED__" else . end ) else . end)' > /tmp/swagger-filter-stage2.json)</tmp/swagger-filter-stage1.json

## drop all "__DEPRECATED__" paths
## or single deprecated methods
(jq 'walk ( if type == "object" then with_entries(select(.value != "__DEPRECATED__")) else . end)' > /tmp/swagger-filter-stage3.json)</tmp/swagger-filter-stage2.json

## tags.json has been manullay tweaked for ordering
TAGS="$(cat 'swagger/tags.json')"

## prepend the tags to swagger body
(jq "$TAGS + ." > swagger/swagger.json) </tmp/swagger-filter-stage3.json
