#!/bin/bash
# writes new configuration file for server

USER=$1 # wallet name stored in ../preprod/wallets/

source env.sh 

UTXO_IN_ADA=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr)) 
echo "UTXO_IN_ADA: $UTXO_IN_ADA"
./create-user-stake.sh reference-wallet
REFERENCE_ADDR=$(cat $WALLET_PATH/reference-wallet.addr)
echo "REFERENCE_ADDR: $REFERENCE_ADDR"

FILE=$REPO_HOME/andamio-config.json

jq -n \
    --arg txRef "$UTXO_IN_ADA" \
    --arg addr "$REFERENCE_ADDR" \
    '{"seedTxRef": $txRef,
    "referenceAddress": $addr}' > $FILE