# Template Haskell

This is the other way of meta-programming (the other is generics)

One use is to get compile time optimization.

## Papers

Why its nice to be Quoted


## Goals

Give programmer more fine-grained control at compile time and run time

Eliminate having to write boiler plate code

Template haskell makes compiling more predictable

Certain monad transformers introduce abstraction overhead which reduces performance.

Template haskell can help to reduce the abstraction load.

With untyped T.H we can generate arbitrary code


We can think of the Q Monad as IO plus a source of new Names.

You can do IO actions during compile time using T.H

You can inspect definition of types and values.