module Establishment ( Establishment(Establishment) ) where

import GpsData

data Establishment = Establishment {
    establishment_location :: Location,
    establishment_name :: String,
    establishment_address :: String
}
instance Show Establishment where
    show e = establishment_name e ++ " at " ++ establishment_address e