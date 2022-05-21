# WEEK 04 PPP 3rd-cohort

### Functors

```haskell
fmap :: (a -> b) -> f a -> f b
```

In the case of IO, if we have a function of type IO A, and a function from A -> B, we can turn IO A to IO B.
Explanation: IO A is a recipe with side-effects that produces an A, if we then take that A and apply the function (A -> B), we have a recipe with the same side-effects that produces B.

Example:

IO A

```haskell
getLine :: IO [Char]
```

A -> B

```haskell
(map toUpper) :: [Char] -> [Char]
```

(this function takes an string and turns it to uppercase)

So

A :: [Char] \
B :: [Char] \
getLine :: IO A

(map toUpper) :: (A -> B)

We can then apply 'fmap' to get something of type IO B

```haskell
fmap (map toUpper) getLine :: IO [Char]
```

### Sequence operator (>>)

Chains two IO operations together ignoring the result of the first one. It's like sticking the two 'recipes' together. One after the other (but ignores the result).

Example:

```haskell
putStrLn "Hello" >> putStrLn "World
```

### Bind operator (>>=)

This operator also chains the 'recipes' but does NOT ignore the result of the first one.

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

this means that if I have a 'recipe' that has side-effects and produces an A, and I have a function that given an A gives me a 'recipe' that also has side-effects and returns a B, then I can combine both to have a 'recipe' that produces a B.

Example of this with IO:

```haskell
getLine :: IO [Char]
putStrLn :: [Char] -> IO ()

getLine >>= putStrLn :: IO ()
```

### Return

This function takes a value and inmediately returns a monad wrapping that value without any side-effects.

```haskell
return :: Monad m => a -> m a
```

### Maybe

The Maybe type, is a type constructor similar to what other languages call Option type.

```haskell
data Maybe a = Nothing | Just a
```

For example, let's say I want to write a function that takes 3 strings, each one representing an integer and I want to return the sum of the 3 numbers. This is a very simple operation but what if any of the strings is an invalid number (NAN). In that case we cannot compute the sum, we need to stop the computation and return an "exception", meaning we can return `Nothing`.

We can use `readMaybe` from `Text.Read` to turn a String into a Int whenever that parse is posible to do:

```haskell
readMaybe :: String -> Maybe Int
```

```haskell
foo :: String -> String -> String -> Maybe Int
foo x y z = case readMaybe x of
    Nothing -> Nothing
    Just k  -> case readMaybe y of
        Nothing -> Nothing
        Just l  -> case readMaybe z of
            Nothing -> Nothing
            Just m  -> Just (k + l + m)
```

In this case, returning `Maybe Int` means we return the sum as an Int if everything went ok, or Nothing if something went wrong (e.g. invalid inputs, string that doesn't represent a valid integer).

### Either

Either is another data type constructor, that takes two data types, and represents the posibility of having one type or the other:

```haskell
data Either a b = Left a | Right b
```

For example:

```haskell
Left "Haskell" :: Either String Int
and also
Right 7 :: Either String Int
```

Following with the example for Maybe (if we want to write a function that takes 3 Strings and returns the sum as an Int), we can have something like this:

```haskell
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "can't parse: " ++ s
    Just a  -> Right a

foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
    Left err -> Left err
    Right k  -> case readEither y of
        Left err -> Left err
        Right l  -> case readEither z of
            Left err -> Left err
            Right m  -> Right (k + l + m)
```

Note that in this case, instead of returning just Nothing when something goes wrong, we can return a String with an error message.

### Logging

Apart from 'error handling' or catching exceptions/failure, we can have the case where we want to log messages while computing a value.

#### Writer

Let's say we have a type constructor called Writer:

```haskell
data Writer a = Writer a [String]
    deriving Show
```

and a function from Int to Writer Int:

```haskell
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]
```

Following with the example of the sum of 3 values, we have:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  Writer (k + l + m) $ xs ++ ys ++ zs
```

where we get the sum of the 3 numbers plus a list of all logs of the numbers that were added in the way.

We can also add the total sum as a log, at the end of the list:

```haskell
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s]
  in
    Writer s $ xs ++ ys ++ zs ++ us
```

where

```haskell
tell :: [String] -> Writer ()
tell = Writer ()
```

Now we can define a bind function:

```haskell
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys
```

and re-write it like this:

```haskell
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []
```

[See more (Monad)](MONAD.md)
