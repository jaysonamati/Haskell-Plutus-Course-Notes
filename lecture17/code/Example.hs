{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example where

import           Data.Aeson     (ToJSON, FromJSON)
import qualified Data.Aeson as  Aeson
import           Data.Binary    (Binary)
import qualified Data.Binary as Binary
import           GHC.Generics   (Generic)


data MyType a b = 
      Flag Bool
    | Combo (a, a)
    | Other b Int (MyType a a)
    deriving (Show, Generic, Binary, ToJSON, FromJSON)

-- instance (Binary a, Binary b) => Binary (MyType a b) (the old way without DeriveAnyClass)
{-

-}

ex1 :: MyType String Bool
ex1 = Other False 42 $ Combo ("Haskell", "Java")