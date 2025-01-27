# Test Transactions

```sh
cabal test all
```

# Andamio SSOI Atlas Back-End Server

## Set Path

Change [REPO_HOME](../../scripts/env.sh#L3) to point to this repository.

## Prepare Server Configuration Files

Ensure you have your configuration files ready. You will need two JSON configuration files:

1- `config-maestro.json` or `config-blockfrost.json` - Core configuration file.
    - `sample.config.json` with maestro information
2- `andamio-config.json` - Andamio specific configuration file.
    - used to identify an instance
    - to initiate a new one use `./00-write-server-config.sh`

Place these files in the root directory of the project.

## Run the Server

To run the server, use the following command:

```sh
cabal run andamio-ssoi-server -- --core-config config-maestro.json --andamio-config andamio-config.json
```

Make sure the paths to your JSON configuration files are correct relative to your current directory.
