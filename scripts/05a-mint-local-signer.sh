#!/bin/bash
# mint local ssoi state for a signer issuer

USER=$1 # wallet name stored in ../preprod/wallets/
ALIAS=$2 # 222 global token must be in USER wallet
ISSUER_NAME=$3 # issuer wallet name stored in ../preprod/wallets/

source env.sh 

ENDPOINT_NAME=mint-local-state-signer
TX_NAME="$TX_PATH/$ENDPOINT_NAME"

UTXO_IN_ADA=$(get_address_biggest_lovelace $(cat $WALLET_PATH/$1.addr)) 
echo "UTXO_IN_ADA: $UTXO_IN_ADA"
USER_ADDR=$(cat $WALLET_PATH/$USER.addr)
echo "USER_ADDR: $USER_ADDR"

ISSUER_PKH=$(cardano-cli conway address key-hash --payment-verification-key-file $WALLET_PATH/$ISSUER_NAME.vkey)
respQ=$(curl -H "Content-Type: application/json" "$SERVER_QUERY_URL/get-index-script-data")
UTXO_IN_222=$(get_UTxO_by_token $(cat $WALLET_PATH/$USER.addr) $(echo $respQ | jq -r '.indexScriptHash') "$(to_hex_tn "222$ALIAS")")
echo "UTXO_IN_222: $UTXO_IN_222"

data=$(jq -n \
        --arg addr "$USER_ADDR" \
        --arg utxoAda "$UTXO_IN_ADA" \
        --arg alias "$ALIAS" \
        --arg utxo222 "$UTXO_IN_222" \
        --arg alias "$ALIAS" \
        --arg issuerPkh "$ISSUER_PKH" \
        '{ "usedAddresses": [$addr]
         , "changeAddress": $addr
         , "collateralTxRef": $utxoAda
         , "user222TxRef": $utxo222
         , "userAlias": $alias
         , "issuerPkh": $issuerPkh}')

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