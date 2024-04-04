# Marlowe

Haskell is pretty good at implementing other languages...

Marlowe is a domain specific language.

A type like this:

```haskell

```

has no meaning by itself.

The way you give it meaning is by writing 'semantic functions': Expr -> ?

## Contracts

A way to formalize contracts such that there is no ambiguity.

Simon Peyton Jones in 2000 tried to formalize financial contracts, in the paper "Composing Contracts: An Adventure in Financial Engineering"

In the paper there is a way to compose two contracts into a more complex contract.

Are there different 'types' of composition? For instance, composing functions and composing contracts seem different.