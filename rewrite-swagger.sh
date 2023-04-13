#!/usr/bin/env bash

## This script requres jq 1.6

set -euo pipefail

PROFILE="${1:-}"
LANG="${2:-"en"}"
SWAGGER_DOWNLOAD_DEFAULT_URI="http://localhost:18083/api-docs/swagger.json"
SWAGGER_INPUT="${3:-"$SWAGGER_DOWNLOAD_DEFAULT_URI"}"

case "$PROFILE" in
    ce)
        TARGET_FILE="redocly/ce-$LANG.json"
        ;;
    ee)
        TARGET_FILE="redocly/ee-$LANG.json"
        ;;
    *)
        echo "Usage $0 ce|ee [en|zh] [SWAGGER_INPUT]"
        echo "The optional parameter SWAGGER_INPUT can be the generated json file"
        echo "Otherwise it downloads the JSON file from $SWAGGER_DOWNLOAD_DEFAULT_URI"
        exit 1
        ;;
esac

## download swagger from EMQX api-docs
if [ -f "$SWAGGER_INPUT" ]; then
    echo "Using swagger JSON file $SWAGGER_INPUT"
    SWAGGER_INPUT_FILE="$SWAGGER_INPUT"
else
    SWAGGER_INPUT_FILE='/tmp/emqx-swagger.json'
    echo "Downloading swagger JSON from $SWAGGER_INPUT"
    curl "$SWAGGER_INPUT" > "$SWAGGER_INPUT_FILE"
fi

# TODO i18n file check

cat $SWAGGER_INPUT_FILE | jq --indent 2 'del(.paths[] | select(any(.[]; .deprecated == true)))' > $TARGET_FILE
