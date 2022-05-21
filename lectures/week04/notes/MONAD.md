# WEEK 04 PPP 3rd-cohort

## Monad

Following with the example of the sum of 3 Int's, we can define:

```haskell
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s
```

where we receive 3 monads/'recipes' of an Int and we return a Monad Int.
Meaning we get 3 'potential' Int's as params and we can potentially return an Int if everything goes ok. 

For the case of the readMaybe, we now have:

```haskell
foo :: String -> String -> String -> Maybe Int
foo x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
```

And for the case of the readEither, we now have:

```haskell
foo :: String -> String -> String -> Either String Int
foo x y z = threeInts (readEither x) (readEither y) (readEither z)
```

For the case of the Writer Monad, we need to define (not needed for Maybe and Either, as they are defined in the Prelude):

```haskell
instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
```

(where `liftM` and `ap` are defined in `Control.Monad`) 

And so, now we can have this:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo x y z = 
    threeInts x y z           >>= \s -> 
    tell ["sum: " ++ show s]  >>
    return s
```


So, in summary, we can think of Monads as a 'super power', where in the case of IO we can have real world side effects like interacting with the OS or printing reading from std IO, etc... in the case of the example of the sum of three integers, we have the power of computing the sum, but at the same time being able to 'fail' (catch the exception/failure) if something goes wrong or add logging of messages as an extra feature while computing the value. 
With `Maybe` we 'fail' returning `Nothing` with `Either` we 'fail' returning `Left` with an error message, etc.



### Do Notation (syntatic sugar)


```haskell
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
```
