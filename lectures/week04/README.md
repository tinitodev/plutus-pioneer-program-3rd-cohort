# WEEK-04 Plutus Pioneer Program (3rd cohort)

## Off-Chain Code

### Monads

Haskell is a "pure functional language" and it has the property of "referential transparency" (in general, there are some exceptions).

This means, that if we have:

```haskell
Foo::Int
Foo = ...
```

It doesn't matter what Foo does, we now that everytime we call Foo is going to return the SAME value, so we can replace every appearance of Foo with this value:

Example:

```haskell
if Foo = 13
```

and we have in our code:

```haskell
......Foo.......Foo......Foo......Foo.......
```

is equivalent to have:

```haskell
......13.......13......13......13.......
```

This is possible because, being pure, Haskell functions are free of side-effects.

But, of course, we need a way to deal with side-effects and I/O operations. That's what "monads" are useful for.
One example of this, is the IO Mondad in Haskell.

For example we can have:
```haskell
Foo :: IO Int
```
which means that now Foo is not a pure-function anymore, but now is like a 'recipe' to calculate a Int that can have side-effects.

Is VERY important to note, that "referential transparency" is maintained and is not broken by this. Now, when invoking/calling Foo we don't get an Int, we get the 'recipe' to calculate that Int.

In Haskell, only the 'main' function (entry point), can execute this so called I/O actions.

### Hello World (in Haskell)

For example the 'Hello World' in Haskell:

```haskell
main :: IO ()
main = putStrLn "Hello world!!"
```

is a 'recipe' that can have side-effects (in this case writting to console) and returns () (Unit), meaning it doesn't return anything, the only important part are the side-effects.


[See more](notes/WEEK_04.md)