# 05

-- Explicit effects

Makes it easier to test programs, or to run then in a different context

The types of plans

Evaluation vs Execution

IO just means a function is allowed to perform side-effects

:: IO () is the one and only plan that actually gets executed

in liftM2 the results are statically known

the bind operator

## Functional Programming with IO

mapM where you want to apply a function to some elements and the function has side-effects.

We can model infinite interactions ....

The simulate fn could go on indefinetly

It showcases how we can test our functions without necessarily using IO

System.Directory is not in base... have to add it to cabal

the hie.yml is needed by hls to configure your project.

cabal install to create executable natively in the computing system.