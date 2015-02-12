module Establishment ( Establishment(Establishment) , establishment_location ) where

import GpsData

data Establishment = Establishment {
    establishment_location :: Location,
    establishment_name :: String,
    establishment_address :: String
} deriving (Eq)
instance Show Establishment where
    show e = establishment_name e ++ " at " ++ establishment_address e