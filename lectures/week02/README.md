# WEEK-02 Plutus Pioneer Program (3rd cohort)

## On-Chain and Off-Chain code

On-chain code is about validation and 'lives' on the blockchain. Off-chain code is on the user/wallet side and produces transactions/interactions with the script address.

## On-Chain code

### Datum, Redeemer, ScriptContext

The low-level representation of these 3 pieces of data (in untyped validation scripts) comes from plutus module PlutusTx.TH, and is called <b>BuiltinData</b> (equivalent to datatype <b>Data</b> defined in plutus-core-0.1.0.0:PlutusCore.Data, but only for on-chain code) [See Docs](https://playground.plutus.iohkdev.io/doc/haddock/plutus-tx/html/PlutusTx.html#t:BuiltinData).

![datatype Data](notes/img/datatype-data.png)

So for <b>Untyped Validator Scripts</b> the function signature would be something like this:

```
mkValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> ()
mkValidator datum redeemer context = 
```
where first BuiltinData is the <b>Datum</b>, the second BuiltinData is the <b>Redeemer</b> and the third BuiltinData is the <b>ScriptContext</b>.