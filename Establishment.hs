{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

module Establishment ( Establishment(Establishment) , establishment_location ) where

import GpsData

data Establishment = Establishment {
    establishment_location :: Location,
    establishment_name :: String,
    establishment_address :: String
} deriving (Eq)
instance Show Establishment where
    show e = establishment_name e ++ " at " ++ establishment_address e
