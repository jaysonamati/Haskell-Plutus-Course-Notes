# Questions on Soko Implementation

- Can we create wallets and v-keys using haskell?
- You always need a datum to spend from a script.
- There is this abstraction called open games, and I wonder if one could embed it in the datum and have interactions between peers mediated by them.
- Each agent's datum keeps track of what 'games' it can engage in for instance Agent A can 'view' (using it's latest datum).
- So each agent has two contracts, one keeping track of all it's resources and one keeping track of how 'others' can interact with its resources...

## Economic simulation (Soko)

- How to create arbitrary wallets that represent various economic actors.
- How to manage how they can interact with each other using datums.
- Does the datum checker inspect the actual datum or the structure of the datum.
- Linearlogy of resources ... i.e track what contracts ("agents") the resource has been through and hence derive some computations about it.
- Can one do parsing inside the contract... like from the datum, parse it and compute something...

                           A
                         /   \
                        /     \
                       /       \
                      B _ _ _ _ C

## Peer Resource Network

- So every peer can add a resource to the network and get 'access' to all 'unowned' network resources.
- Each resource is controlled by a smart-contract?
- Each peer controls a smart-contract?
- Datum has information on whether a peer can interact with a resource?

## Useful comments

- Can have ratios of contributions...
- If you have many people then the calculations might involve many steps.
- Agents could be one contract (NFT per agent state)
- World validator...

## Comonads

If you have join then you have bind

A combination of Monad and Comonad using A free Monad and A free Comonad
The above can be used to model interaction.