# More on Monads

- Revisiting Monads
- Examples of Monads
- Combining monads

Applicative enable to embed a to fa, 
effectful function, effectful argument gives effectful result

IF you put monads in the kleisli format, the laws become 
nice.

## Examples

- Either e: failure with message e
- List: indeterminism
- State s: Modifiable state of type s
- IO: Real-world side effects
- STM: Allows high-level dealing with threads etc
- Gen: Generating random values
- Parser t : parses strings of type t

There are general functions that work for every Monad.

Although there are special functions for certain Monads, for
instance:

```haskell
get :: State s s
put :: s -> State s ()
```

## Type families

classes can have methods
and they can also have associated types...

## Combining Monads

One can combine monads but it's not as straightforward as combining functors and applicatives

what we do in the case of monad is transform one monad into another.

Monad transfomers are not so elegant since the underlying theory is not beautiful, this leads to several restrictions and complications. 

(How about using the Kleisli categories or monads to combine effects ???)