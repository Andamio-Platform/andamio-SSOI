#!/bin/bash
# add rewards to prove credential validator which can be unlocked if credentials are met

USER=$1 # wallet name stored in ../preprod/wallets/
OWN_ALIAS=$2 # 222 global token must be in USER wallet

source env.sh 

ENDPOINT_NAME=add-rewards
TX_NAME="$TX_PATH/$ENDPOINT_NAME"

UTXO_IN_ADA=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$USER.addr)) 
echo "UTXO_IN_ADA: $UTXO_IN_ADA"
USER_ADDR=$(cat $WALLET_PATH/$USER.addr)
echo "USER_ADDR: $USER_ADDR"

reqSigner=$(jq -n --arg ownerAddr "$USER_ADDR" '{"ownerAddress": $ownerAddr}')
respLSSigner=$(curl -H "Content-Type: application/json" -d "$reqSigner" "$SERVER_QUERY_URL/get-local-state-signer-data")
reqToken=$(jq -n --arg ownerAlias "$OWN_ALIAS" '{"ownerAlias": $ownerAlias}')
respLSToken=$(curl -H "Content-Type: application/json" -d "$reqToken" "$SERVER_QUERY_URL/get-local-state-token-data")

data=$(jq -n \
        --arg addr "$USER_ADDR" \
        --arg utxoAda "$UTXO_IN_ADA" \
        --arg issuerAlias "$OWN_ALIAS" \
        --arg lsSignerCs "$(echo $respLSSigner | jq -r '.scriptHash')" \
        --arg lsTokenCs "$(echo $respLSToken | jq -r '.scriptHash')" \
        '{ "usedAddresses": [$addr]
         , "changeAddress": $addr
         , "collateralTxRef": $utxoAda
         , "requiredIntCredentials": [[$lsTokenCs, 1]]
         , "requiredStrCredentials": [[$lsSignerCs, ["test1"]]]
         , "lovelaceRewards": 5000000
         , "tokensToDeposit": []}')

echo $data | jq

resp=$(curl -H "Content-Type: application/json" -d "$data" "$SERVER_TX_URL/$ENDPOINT_NAME")

#echo $resp

cat $EMPTY_TX_PATH | jq --arg ch "$(echo $resp | jq -r '.unsignedTxCBOR')" '.cborHex=$ch' > $TX_NAME.raw

cardano-cli conway transaction sign \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-body-file $TX_NAME.raw \
    --out-file $TX_NAME.sign \
    --signing-key-file $WALLET_PATH/$USER.skey

cardano-cli conway transaction submit --testnet-magic ${TESTNET_MAGIC} --tx-file $TX_NAME.sign