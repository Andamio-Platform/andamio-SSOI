#!/bin/bash

source env.sh

USER=$1 # wallet name that will be stored in ../preprod/wallets/
USER_WALLET_PATH=$WALLET_PATH/$USER

cardano-cli address key-gen \
      --verification-key-file $USER_WALLET_PATH.vkey \
      --signing-key-file $USER_WALLET_PATH.skey

cardano-cli conway stake-address key-gen \
    --verification-key-file $USER_WALLET_PATH-stake.vkey \
    --signing-key-file $USER_WALLET_PATH-stake.skey

cardano-cli conway stake-address build \
    --testnet-magic ${TESTNET_MAGIC} \
    --stake-verification-key-file $USER_WALLET_PATH-stake.vkey \
    --out-file $USER_WALLET_PATH-stake.addr

cardano-cli address build \
    --payment-verification-key-file $USER_WALLET_PATH.vkey \
    --stake-verification-key-file $USER_WALLET_PATH-stake.vkey \
    --testnet-magic ${TESTNET_MAGIC} \
    --out-file $USER_WALLET_PATH.addr

