# Concurrency And Parallelism

Concurrency is about having different threads of control.
Parallelism is about speeding stuff up.

We will talk about concurrency through implementing

The threads produced by `forkIO` are in the haskell runtime system and are lightweight hence you could have hundreds or thousands of them

All threads are killed if the main thread is killed.

The execution of each thread is always non-deterministic.

## Networking

We use a library called `network-simple` can talk to servers exchanging bytes

`network` is more low level and gives more control.

all threads are 'independent' of each other, i.e closing one doesn't make the rest of then close...

generate a client??

You can generate code that acts as the client.

### Communicating between threads

You could have shared memory

Or you could use message passing.

#### Shared memory

This is done mostly using `IORef a`.

This contains a mutable variable `a`

The cell stores thunks... i.e it will only evaluate the expression in there when needed.

bang patterns force evaluation

What does evaluation mean?
Thunks : Unevaluated expression
it means WHNF (Weak Head NOrmal Form ) only evaluates to the top-most constructor
ALso thunks take space so one might need to strategically evaluate them.

The atomicModifyIORef' function will read the IORef value and produce

We can use locking using MVars..

Nested threads are a thing...

#### STM

IT's like io but only reads and writes variable
like a restricted version of IO


`retry` basically starts the entire STM computation if the value has changed ()
`orElse` tries the second if the first retries


wait is a way to detect whether they are errors in the thread as it propagates them to the main thread.


##### Some ghci 