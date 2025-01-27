#!/bin/bash

export REPO_HOME="$HOME/Desktop/Gimbalabs/andamio-SSOI" # path to this repository
export NETWORK_DIR_PATH="$REPO_HOME/preprod"            # path to network in use like preprod
export TESTNET_MAGIC=1

export TX_PATH="$NETWORK_DIR_PATH/tx"
export EMPTY_TX_PATH="$TX_PATH/empty-tx.raw"
export WALLET_PATH="$NETWORK_DIR_PATH/wallets"

export SERVER_URL="http://localhost:8081"
export SERVER_TX_URL="$SERVER_URL/tx"
export SERVER_QUERY_URL="$SERVER_URL/query"

export GREEN=$(tput setaf 2)
export BLUE=$(tput setaf 4)
export CYAN=$(tput setaf 6)
export NORMAL=$(tput sgr0)

# $1 = wallet name
balanceUser() {
    printf "\n${GREEN}$1${NORMAL}\n"
    ./balance.sh $1
}

# $1 = clear token name string
to_hex_tn() {
    printf '%s' "$1" | xxd -p
}

# $1 = address
get_address_biggest_lovelace() {
    cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $1 --out-file utxos.tmp
    max_utxo=$(cat utxos.tmp | jq 'with_entries(select((.value.value | length) == 1)) | to_entries | max_by(.value.value.lovelace)')
    rm utxos.tmp
    echo $(echo $max_utxo | jq -r '.key')
}

# $1 = address
# $2 = currency symbol
# $3 = token name
get_UTxO_by_token() {
    cardano-cli query utxo --testnet-magic ${TESTNET_MAGIC} --address $1 --out-file utxos.tmp
    token_utxo=$(cat utxos.tmp | jq --arg cs $2 --arg tn $3 'to_entries[] | select(.value.value[$cs][$tn]>=1)')
    rm utxos.tmp
    echo $(echo $token_utxo | jq -r '.key')
}
