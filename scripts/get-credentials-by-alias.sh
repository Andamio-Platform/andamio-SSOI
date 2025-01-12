#!/bin/bash

OWN_ALIAS=$1 # any alias

source env.sh 


reqR=$(jq -n --arg ownerAlias "$OWN_ALIAS" '{"alias": $ownerAlias}')
respR=$(curl -H "Content-Type: application/json" -d "$reqR" "$SERVER_QUERY_URL/get-credentials-by-alias")
echo $respR | jq