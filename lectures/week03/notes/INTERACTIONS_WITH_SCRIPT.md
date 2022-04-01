# WEEK 03 PPP 3rd-cohort

## Deploying Script to Testnet

### Interacting with the Vesting Script

### Give

Let's say wallet-01 wants to Give ADA.
To perform the 'give' action, we can just create a tx, where we send ada from wallet-01 to the script address, and we add the Datum hash with '--tx-out-datum-hash-file' (in our case datum is just Unit ()).

```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in abae0d0e19f75938537dc5e33252567ae3b1df1f35aafedd1402b6b9ccb7685a#0 \
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file unit.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
```

We can check the funds arrived at the script:

```bash
$ cardano-cli query utxo --address $(cat vesting.addr) --testnet-magic 1097911063
```

### Grab

Now wallet-02 wants to grab the ADA .
To perform a 'grab' action, we create a tx that spends the utxo sitting at the script. As this is a spending tx, we need to provide the actual plutus script with '--tx-in-script-file'. We also need to provide the actual Datum (and not only the hash) with '--tx-in-datum-file' (in our case is just Unit). And the Redeemer with '--tx-in-redeemer-file' (which in our case is also Unit). \
We need to specify a 'collateral' just in case something goes wrong (only needed when executing plutus scripts, but very rarely used, just for some unpredicted cases). We do this with '--tx-in-collateral' and in this case we provide a utxo from wallet-02.
Additionally, we need to pass the '--required-signer-hash' (in our case is the pubkeyhash we already computed for wallet-02).

The protocol params can be query like this:

```
$ cardano-cli query protocol-parameters --testnet-magic 1097911063 --out-file protocol.json
```

And for the '--invalid-before' slot, we can just pass the current slot, and we can query it like this:

```
$ cardano-cli query tip --testnet-magic 1097911063 
```

```
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in 18cbe6cadecd3f89b60e08e68e5e6c7d72d730aaa1ad21431590f7e6643438ef#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 18e93407ea137b6be63039fd3c564d4c5233e7eb7ce4ee845bc7df12c80e4df7#1 \
    --required-signer-hash c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a \
    --invalid-before 48866954 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
```

We can check the funds arrived at wallet-02:

```bash
$ cardano-cli query utxo --address $(cat 02.addr) --testnet-magic 1097911063
```
