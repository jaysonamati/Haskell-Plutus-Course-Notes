# Optics

If we compose a Lens with a Traversal we get a traversal

>> `view each ["Haskell", "Idris", "Adga"]`
toListOf each tree
toListOf (each . each) tree


Prisms is for stuff that might not be there ... 


Optics focus on one substructure and Traversals focus on substructures of the same type. 

you can compose lens and traversals... they all become traversals 