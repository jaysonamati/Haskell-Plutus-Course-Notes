# MoreDataStructures

If you need a lot of updates it's a good idea to avoid
vectors, finite maps and sequences are better suited for 
this.

Vectors shine at random access.

Vectors have a well defined length

## Unboxed types

`Int#` is built-in in the compiler, it is an unboxed type.

- They are faster and take less memory and have no  
    indirection

Boxed types support polymorphism, and laziness.

We can't use unboxed for id because of the different kinds of unboxed types

Levity-polymorphism works both for unboxed and boxed types.

Check out the STMonad that it's like the IOMonad but one can only update some data structure and not the RealWorld.