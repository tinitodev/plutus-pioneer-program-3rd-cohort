# WEEK 03 PPP 3rd-cohort

## Vesting example (using ScriptContext)


In this example the validator script receives a PaymentPubKeyHash (payment public key hash) and a POSIXTime as Datum. The PubKeyHash is indicating the beneficiary (the person that has permissions to withdraw the funds) and the POSIXTime is the deadline (in this case is indicating that the beneficiary can only withdraw AFTER the deadline is reached).

```
data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
```

As you can see, we use two helper functions (signedByBeneficiary and deadlineReached) to check both conditions for a withdraw, and in both of those functions we need the TXInfo field from the ScriptContext. 

For checking who signed the Tx we can use the already defined 'txSignedBy' that receives a TXInfo and a PubKeyHash and returns true if the Tx was signed by the owner of the PubKeyHash. We have to 'unwrap' it with unPaymentPubKeyHash to turn PaymentPubKeyHash into simply PubKeyHash (although the underlying type is the same, PaymentPubKeyHash is just a wrapper to PubKeyHash to be able to make a distinction).

```
type PaymentPubKeyHash :: *
newtype PaymentPubKeyHash = PaymentPubKeyHash {unPaymentPubKeyHash :: PubKeyHash}
-- Defined in ‘Ledger.Address’
```

To check the deadline was reached we can just ask if the txInfoValidRange is contained in the interval from deadline to infinity (from $ deadline dat), that means the Tx is happening anytime after the deadline (cause deadline is the lowerBound of the interval that contains it).


![Deadline interval](img/deadline-interval.png)


