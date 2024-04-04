# Testing

Convince other parties that program is correct.
Testing cannot prove the absence of bugs.

What is a program correct? When it follows the specification
How to establish a relation between  the specification and implementation.

Do types free us from the need to test?

- There are limits to what the types can express
- Because we have type we can test interesting properties

In dependent types it is possible to specify a program completely with dependent types.

## QuickCheck

type-driven testing with random test case generation

The tests are type-checked.

By John hues and Koen Claessen
ComputerPhile john hues episode

A goog specification is
    - as precise as necessary,
    - no more precise than necessary

Some properties automatically imply other properties...

The spec should distinguish from all other sorting programs

When using your own type do you have to make them instances of testable?

Check that the tests also have bugs?

What's the other property??
 For lists its permutation.

## How QuickCheck works

quickCheck generates random lists or other types mentioned in properties and checks them if they are True.

In practice Use holes to help the compiler be a useful companion.

It also works for polymorphic arguments e.g a -> a -> Bool

It also works for cases

The testable functions

We can think of Property as a generalization of Bool.

If type a has type class Arbitrary then you can generate random a's

with verboseCheck we get to see what it tried to get to the minimal example.

We can have some statistics of the random test cases using the collect function.

The chance of generating a sorted list randomly is small.

the `Gne a` specifies how to actually generate the types...

In case of custom types we have to implement our own generators

A type can have exactly on e instance of a typeclass although you can use newtype to define other instances of a typeclass

QuckCheck has all these newtype wrappers for when we want samples with a specific character....

### Program Coverage

-- Shows which part of the program are covered by the test.

### Random stuff

-- Testing in blockchain, how proof of stake consensus is tested.

-- Property has many interesting functions for doing stuff ... check it out !!!
