# WEEK 03 PPP 3rd-cohort

## Deploying Script to Testnet

### Serializing the Plutus Script

In order to serialize the plutus code and write a 'binary' to disk we use the Cardano.Api haskell library (which is the same that cardano-cli uses under the hood).

We need to provide the params to our parameterized script (here "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a" would be 02.pkh, and the deadline in POSIX time can be calculated using an [epoch converter](https://www.epochconverter.com/)):

```haskell
writeVestingValidator :: IO (Either (FileError ()) ())
writeVestingValidator = writeValidator "testnet/vesting.plutus" $ validator $ VestingParam
    { beneficiary = Ledger.PaymentPubKeyHash "c2ff616e11299d9094ce0a7eb5b7284b705147a822f4ffbd471f971a"
    , deadline    = 1643235300000
    }
```

where writeValidator is

```haskell
writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript
```

For the Data (Datum, Redeemer), we need to convert from Plutus 'Data' datatype to what we call 'ScriptData' datatype, which is the one used by Cardano.Api.

```haskell
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs
```

And then we take that Data and serialize and write it into a json file. So this writeJSON function takes that Plutus Data, converts it to ScriptData and then serialize it and writes it to a file.

```haskell
writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData
```

In this case for our Vesting example we only need Unit (), both for the Datum and the Redeemer, so we can just call writeJSON with a file-path and ():

```haskell
writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()
```

Now that we have generated the binary (serialized script) in 'vesting.plutus' file, we can generate a script-address for our script:

```bash
$ cardano-cli address build-script --script-file vesting.plutus --testnet-magic 1097911063 --out-file vesting.addr
```