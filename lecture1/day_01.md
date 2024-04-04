# Session notes

## Intro

-Lecture
    - Crypto
    - Haskell
-Exercises
-Tests

## Course schedule Explanation

-Every week an assignment
-solved in teams (4 max)
-Individual tests
-large projects nad demos

## Expectations Clarification

-Assignments
-Ask a lot of questions
-Ideas for course content
-Feedback

## Communication

-Discord
-Stack overflow
-github

### Haskell and crypto (Dr lars IOG)

#### Blockchain

Classically based on a ledger (basically an long list of transactions maintained by a trusted person)

There is a linear order of the 'blocks'

Ledgers are usefule in finance, property deeds, supply chains, diplomas (GUNET)

Tamper-proof records

Doesn't solve all problems

#### Crypto currencies

Units of currency are associeated with public keys

##### Hash function

Takes arbitrary bitstrings and converts to a fixed length.

- effectively computable
- collision free (mathematically impossible)
- hiding
- (puzzle friendly)

##### Digital signatures

- Key generation
- Signing
- Verification

##### Consensus protocols

Proof of stake; basically like a lottery where a lovelace is picked to create consensus and whoever owns the coin gets rewarded

##### Haskell 

-extremely high level of abstraction
-expressive and terse 
-perfect for writing DSL (protocols)
-statically typed with a sophisticated type system
-is fun

##### Questions

How to make checking of balances in previous blocks efficient? Indexing (basically an SQL d.b)

## Haskell

-algebraic datatypes
-type inference
-type classes
-explicit effects
-lazy evaluation

#### Types

- Data constructors are a type of function


##### Modularization

E.g you can separate into a part that generates all solutions and another part for checking the solutions.


##### Exercises

- How to define datatypes.
- How datatypes shape functions (type-directed programming).: