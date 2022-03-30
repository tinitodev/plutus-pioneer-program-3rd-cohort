# WEEK 02 PPP 3rd-cohort

## Off-Chain Code

This part of the code, doesn't need to be in Haskell, it can be written in any language.
This code is only to interact with the script address but it doesn't take part of the on-chain validator script (smart contract). Is not even mandatory, as you could interact with the script directly through the cardano-cli or other tools.
Having said that, it is pretty convinient to use Haskell and take advantage of many fuctions/librarires provided by the Plutus team.


### Schema

In Haskell we generally define this Schema type for the Endpoints we want to 'expose' to the 'user'.\
For example, we can define:

```haskell
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()
```

where 'give' and 'grab' are the two endpoints/actions we want to provide to the user (people interacting with this contract).


### Endpoints

After defining the Schema, we need to define the actual behaviour for this endpoints:

```haskell
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount
```

here <i>give</i> receives an amount (to send) and defines a tx with the Constraint  ``mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount`` meaning the output is to a 'script address' with the hash of the one we defined earlier (valHash), the Datum in this case doesn't matter (is an arbitrary one), and the amount in lovelace received as a parameter. \
Then we submit the Tx, wait for confirmation and print a log message.

```haskell
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"
```

here <i>grab</i> has no parameters. It looks at all the utxo's sitting at the scriptAddress, takes their reference (oref) and then 'lookups' is providing the validator (remember the spending transaction needs to provide the actual validator). \
We build the Tx concatenating Constraints for each utxo (saying that needs to be spent), and we also provide a Redeemer (which in this case is not used, so is an abritrary one). \
Then we submit the Tx providing not only the tx itself, but also the lookups (containing the validator script). We wait for confirmation, and log a message.


### Finally

We can combine all this functionality into this <b>endpoints</b> function, making available the endpoints and blocking execution waiting for the user to choose and provide parameters when needed or just pressing continue.

```haskell
endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab
```
