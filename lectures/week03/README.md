# WEEK-03 Plutus Pioneer Program (3rd cohort)

## Script Context

The ScriptContext type is defined in package plutus-ledger-api, in module Plutus.V1.Ledger.Contexts ([see docs](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Contexts.html))

![ScriptContext datatype](notes/img/scriptcontext.png)

where the <b>ScriptPurpose</b> can be:

![ScriptPurpose datatype](notes/img/scriptpurpose.png)

<b>Spending</b> is the most commmon one, and refers to the case where the script is run to validate that we can spend certain inputs in a transaction.    \
<b>Minting</b> is used for defining minting/burning policies for native tokens.  \
<b>Rewarding</b> is related to staking rewards. \
<b>Certifying</b> is used in delegation certificates, etc.

The <b>TXInfo</b> consist of:

![TXInfo datatype](notes/img/txinfo.png)

## Time Handling

Before even running the validator script, the Cardano Node always checks first that the current time falls into the valid time range provided in the Tx (<b>txInfoValidRange</b> of type POSIXTimeRange). So the time is only a pre-check, that means the validator script itself is deterministic, and the time-check is independent. For this reason, the result of the validation does not depend on when it is run (wheter it is run in the wallet before submission or in one of the nodes when validating a tx). \
This means that, during the execution of a validator script, we don't have to worry about the time-check, and we can asume that the time falls into the valid range (as it was already checked). \
By default, all transactions use the 'infite time range' meaning they are always valid no matter what time they arrive at the node for validation.

POSIXTimeRange

```
typetype POSIXTimeRange = Interval POSIXTime
```

where Interval:

```
Interval	 
ivFrom :: LowerBound a	 
ivTo :: UpperBound a	 
```

and LowerBound:

```
LowerBound (Extended a) Closure	 
```

where Closure is just a Bool to specify if we include the boundary or not and Extended is either:
```
NegInf 
Finite a
PosInf 
```
(-/+ infinite, or Finite a)

Useful functions:

```
member :: Ord a => a -> Interval a -> Bool
(checks if a given time is included in the interval)

interval :: a -> a -> Interval a
(returns a interval with the two parameters as included upper/lower boundaries)

from :: a -> Interval a
(interval from parameter to eternity)

to :: a -> Interval a
(interval from genesis to parameter)

intersection :: Ord a => Interval a -> Interval a -> Interval a
('intersection a b' is the largest interval that is contained in a and in b, if it exists)

overlaps :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
(Check whether two intervals overlap, that is, whether there is a value that is a member of both intervals)

contains :: Ord a => Interval a -> Interval a -> Bool
(a contains b is true if the Interval b is entirely contained in a. That is, a contains b if for every entry s, if member s b then member s a)
```

[See docs](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Extended)

Example of Intervals with Integers:
```
Prelude> import Plutus.V1.Ledger.Interval

Interval
[10, 20]
Prelude> interval (10 :: Integer) 20
Interval { ivFrom = LowerBound (Finite 10) True, ivTo = UpperBound (Finite 20) True }

(means Integer interval from 10, included, to 20 included)


Member

Prelude> member 9 $ interval (10 :: Integer) 20
False
Prelude> member 12 $ interval (10 :: Integer) 20
True
Prelude> member 10 $ interval (10 :: Integer) 20
True
Prelude> member 20 $ interval (10 :: Integer) 20
True
Prelude> member 21 $ interval (10 :: Integer) 20
False
Prelude> member 9 $ interval (10 :: Integer) 20
False
Prelude> member 10 $ interval (10 :: Integer) 20
True
Prelude> member 12 $ interval (10 :: Integer) 20
True
Prelude> member 20 $ interval (10 :: Integer) 20
True
Prelude> member 21 $ interval (10 :: Integer) 20
False


From 
[30 , +Infinite)
Prelude> from (30 :: Integer)
Interval {ivFrom = LowerBound (Finite 30) True, ivTo = UpperBound PosInf True}

Prelude> member 21 $ from (30 :: Integer)
False
Prelude> member 30 $ from (30 :: Integer)
True
Prelude> member 3000 $ from (30 :: Integer)
True


To
(-Infinite to 30]
Prelude> to (30 :: Integer)
Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite 30) True}

Prelude> member 31 $ to (30 :: Integer)
False
Prelude> member 30 $ to (30 :: Integer)
True
Prelude> member 29 $ to (30 :: Integer)
True
Prelude> member 7 $ to (30 :: Integer)
True
Prelude> member (-7) $ to (30 :: Integer)
True


Intersection
[10,20] inter [18, 30] = [18, 20]

Prelude> intersection (interval (10 :: Integer) 20) $ interval 18 30
Interval {ivFrom = LowerBound (Finite 18) True, ivTo = UpperBound (Finite 20) True}


Contains and Overlaps

Prelude> contains (to (100 :: Integer)) $ interval 30 80
True
Prelude> contains (to (100 :: Integer)) $ interval 30 100
True
Prelude> contains (to (100 :: Integer)) $ interval 30 101
False
Prelude> overlaps (to (100 :: Integer)) $ interval 30 101
True
Prelude> overlaps (to (100 :: Integer)) $ interval 101 110
False
```