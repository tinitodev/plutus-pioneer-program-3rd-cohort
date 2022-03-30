# WEEK 02 PPP 3rd-cohort

## High Level, Typed Validation Scripts

### Datum, Redeemer, ScriptContext

In order to represent values more accurately in a more 'strongly-typed' fashion, that better represent our business logic, we can provide more specific types other than BuiltinData for datum, redeemer and context (although this is a more clear and powerful way to work, it could come along with some performace/resources impact).

Example:

```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

You can see that for Datum and Redeemer we can use any type that best suits our validator needs, and for Context we have the ScriptContext type already defined. As for the returning type, instead of () used for untyped validator scripts, now we use a more intuitive/declarative Bool, meaning that: if we return true, then the validation is OK, and if we return false (or there is an Error) then the validation failed.

We can use ``traceIfFalse`` to trace/log when certain validation is false. This is specially helpful to know what went wrong.

Also, as you can see below there is some extra boilerplate needed for typed validators:

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

First of all we need to introduce a new type that encodes the types of the Datum and Redeemer (here Scripts.ValidatorTypes now comes from Ledger.Typed.Scripts). \
And then for compiling to PlutusCore as before we need to use Scripts.mkTypedValidator @Typed, but with the addition of Scripts.wrapValidator @() @Integer that takes the Datum and Redeemer as BuiltinData, and turns them into () and Integer. This wrap function is used to 'translate' untyped to typed validators.

(In 'PlutusTx.isData.Class' we can find 'toData' and 'fromData' functions that are used to translate from/to BuiltinData, back and forth, where we defined how to 'serialize' that data)

Examples:

```haskell
toData () -> Constr 0 []
fromData (Constr 0 []) :: Maybe ()-> Just ()

fromData (Constr 4 []) :: Maybe ()-> Nothing

toData (42 :: Integer) -> I 42
fromData (I 42) :: Maybe Integer -> Just 42

fromData (List []) :: Maybe Integer -> Nothing
```

In order to use the isData Class with our own custom types, we need to add the following to add an instance of this class for our type (defined in-place at compile time)

```haskell
--Example, we have:
newtype MySillyRedeemer = MySillyRedeemer Integer

--We need to add:
PlutusTx.unstableMakeIsData ''MySillyRedeemer
```

(Note: for quick testing or trying things we can use this unstableMakeIsData, but for production we should use the stable version, were you define the Constr index, and so in that way we can guarantee compatibility between different Plutus versions)


[See more (off-chain code)](WEEK_02_OFF_CHAIN_CODE.md)
