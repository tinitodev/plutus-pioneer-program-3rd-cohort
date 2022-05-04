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





