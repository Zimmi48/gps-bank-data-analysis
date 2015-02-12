module GooglePlaceRequest ( findEstablishment , placeEstablishments ) where

import Data.Maybe
import Data.List
import Control.Monad
import Network.HTTP.Conduit
import Network (withSocketsDo)
import JsonInputReader
import GpsData
import BankData
import Establishment

-- Attention: the number of request per day is limited to 1000
-- It can be reached very rapidly!
findEstablishment :: Double -> Place -> Transaction -> IO (Maybe Establishment)
findEstablishment accuracy place trn = withSocketsDo $
    liftM (listToMaybe . getEstablishments) $ simpleHttp url
    where
        url = base_url ++ location ++ radius ++ keyword ++ types ++ key
        base_url = "https://maps.googleapis.com/maps/api/place/search/json?"
        location = "location=" ++ showLocation (place_center place) ++ "&"
        radius   = "radius="   ++ show (max 100 $ 2*accuracy)       ++ "&"
        keyword  = "keyword="  ++ name trn                          ++ "&"
        types    = "types=establishment&"
        key      = "key=" ++
        -- this can be changed

placeEstablishments :: Int -> Double -> Place -> [Transaction] -> IO [Establishment]
placeEstablishments maxRequests accuracy place trns =
    liftM catMaybes $
    sequence $
    map (findEstablishment accuracy place) (take maxRequests trns)
-- even if there are more transactions associated to one given place
-- we do not allow for a very large number of API requests per place


