
module ScratchPad where 

data Chain = GenesisBlock | Block Chain Txs deriving (Show, Eq)

type Txs = Int

chain1 :: Chain 
chain1 = Block GenesisBlock 2

chain2 :: Chain 
chain2 = Block chain1 4

chainLength :: Chain -> Int 
chainLength GenesisBlock = 0
chainLength (Block c _n) = 1 + chainLength c

hasBlock :: Txs -> Chain -> Bool
hasBlock _ GenesisBlock = False
hasBlock n (Block c t) = n == t || hasBlock n c

everyBlock :: Txs -> Chain -> Bool
everyBlock _ GenesisBlock = False
everyBlock n (Block c t) = n == t && everyBlock n c

(|>) :: Chain -> Txs -> Chain 
(|>) = Block

hasBlockProp :: (Txs -> Bool) -> Chain -> Bool
hasBlockProp _ GenesisBlock = False
hasBlockProp p (Block c t)  = p t || hasBlockProp p c

data Chain' txs = GenesisBlock' | Block' Chain txs deriving 

build :: Int -> Chain
build n = if n < 0
    then GenesisBlock
    else Block (build (n - 1)) n


addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c