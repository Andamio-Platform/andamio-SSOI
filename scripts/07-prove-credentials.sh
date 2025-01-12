#!/bin/bash

USER=$1 # wallet name stored in ../preprod/wallets/
OWN_ALIAS=$2 # 222 global token must be in USER wallet

source env.sh 

ENDPOINT_NAME=prove-credentials
TX_NAME="$TX_PATH/$ENDPOINT_NAME"

UTXO_IN_ADA=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr)) 
echo "UTXO_IN_ADA: $UTXO_IN_ADA"
USER_ADDR=$(cat $WALLET_PATH/$USER.addr)
echo "USER_ADDR: $USER_ADDR"
respQ=$(curl -H "Content-Type: application/json" "$SERVER_QUERY_URL/get-index-script-data")
UTXO_IN_222=$(get_UTxO_by_token $(cat $WALLET_PATH/$USER.addr) $(echo $respQ | jq -r '.indexScriptHash') $(to_hex_tn "222$OWN_ALIAS"))
reqR=$(jq -n --arg ownerAlias "$OWN_ALIAS" '{"alias": $ownerAlias}')
respR=$(curl -H "Content-Type: application/json" -d "$reqR" "$SERVER_QUERY_URL/get-possible-reward-tx-refs")
rewardTxRef=$(echo $respR | jq -r '.rewardTxRefs[0]')
echo "respR: $respR"

data=$(jq -n \
        --arg addr "$USER_ADDR" \
        --arg utxoAda "$UTXO_IN_ADA" \
        --arg utxo222 "$UTXO_IN_222" \
        --arg rewardTxRef "$rewardTxRef" \
        --arg alias "$OWN_ALIAS" \
        '{ "usedAddresses": [$addr]
         , "changeAddress": $addr
         , "collateralTxRef": $utxoAda
         , "user222TxRef": $utxo222
         , "rewardTxRef": $rewardTxRef
         , "alias": $alias}')

echo $data | jq

resp=$(curl -H "Content-Type: application/json" -d "$data" "$SERVER_TX_URL/$ENDPOINT_NAME")

echo $resp

cat $EMPTY_TX_PATH | jq --arg ch "$(echo $resp | jq -r '.unsignedTxCBOR')" '.cborHex=$ch' > $TX_NAME.raw

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $TX_NAME.raw \
    --out-file $TX_NAME.sign \
    --signing-key-file $WALLET_PATH/$USER.skey

cardano-cli conway transaction submit --testnet-magic ${TESTNET_MAGIC} --tx-file $TX_NAME.sign