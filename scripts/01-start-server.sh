#!/bin/bash
# start atlas server with given configuration files

cd ..
cabal run andamio-ssoi-server -- --core-config config-blockfrost.json --andamio-config andamio-config.json
# cabal run andamio-ssoi-server -- --core-config config-maestro.json --andamio-config andamio-config.json
