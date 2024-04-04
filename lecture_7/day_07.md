# Abstraction patterns

Introducing `Monad` and `Applicative`

Try to identify patterns that can be turned into general functions.

Monad is a special computation with some thrills.
It's a way to combine simpler explanations into more complex operations.
Gives different meanings of how to combine two statements together.
One can combine different monads
The do notation works for any monad...

Makes it easier to maintain code.

Applicative can help one parallelize their computations.

Gen from QuickCheck is also a monad

Newer ghc versions don't have return.

The associativity laws also apply to do block (i.e where they start and end)

Proof the associativity law for the Maybe monad?

The whole once you are in impure haskell you stay there only applies to IO and not to other Monads.

Internally IO can be thought of a state Monad where it takes the RealWorld as State.

With one failure everything fails...

mapM_ means we throw away the effects (or the special containers in regard to other monads)

`forM` and `mapM` are similar except with forM we can use functions over several lines.... 
