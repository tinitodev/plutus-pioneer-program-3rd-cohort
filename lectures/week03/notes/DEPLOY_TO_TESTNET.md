# WEEK 03 PPP 3rd-cohort

## Deploying Script to Testnet

We need to be running a cardano-node/cardano-cli in order to submit txs to the blockchain.

So we need to clone the repos and compile the code or just download binaries from (for example) Hydra. One way or another, we need to have cardano-node and cardano-cli binaries installed on our machine.

### Start the Node

Then we can start the node by running:

```bash
cardano-node run \
 --topology testnet-topology.json \
 --database-path db \
 --socket-path node.socket \
 --host-addr 127.0.0.1 \
 --port 3001 \
 --config testnet-config.json
```

(the first time we need to wait for the Node to download and synchronize the whole blockchain history)


### Create Wallets

Generate verification/signing key-pair for wallet 01:
```bash
$ cardano-cli address key-gen --verification-key-file 01.vkey --signing-key-file 01.skey
```

Generate verification/signing key-pair for wallet 02:

```bash
$ cardano-cli address key-gen --verification-key-file 02.vkey --signing-key-file 02.skey
```

Create a payment address asociated with verification key 01 (no need for staking part, just payment address):

```bash
$ cardano-cli address build --payment-verification-key-file 01.vkey --testnet-magic 1097911063 --out-file 01.addr
```

Create a payment address asociated with verification key 02:

```bash
$ cardano-cli address build --payment-verification-key-file 02.vkey --testnet-magic 1097911063 --out-file 02.addr
```

### Fund Wallets 

Now that we have generated 2 addresses, let's just fund them requesting some tAda from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)

We can check that the funds arrived by querying the blockchain:

```bash
$ cardano-cli query utxo --address $(cat 01.addr) --testnet-magic 1097911063
```

### Send tADA

To send tADA from one address to the other, we build the Tx:

```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in dfc1a522cd34fe723a0e89f68ed43a520fd218e20d8e5705b120d2cedc7f45ad#0 \
    --tx-out "$(cat 02.addr) 10000000 lovelace" \
    --out-file tx.body
```

then sign it:

```bash
cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed
```

and finally submit it:

```bash
cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
```

Also, before we continue, we need to get the PubKeyHash of the 02 wallet/address to use it as a parameter to our Vesting parameterized script.

```
cardano-cli address key-hash --verification-key-file 02.vkey --out-file 02.pkh
```

[See more (serialize plutus script)](SERIALIZE_PLUTUS_SCRIPT.md)