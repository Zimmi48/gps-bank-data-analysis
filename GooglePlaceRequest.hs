module GooglePlaceRequest (findEstablishment) where

import Data.Maybe
import Control.Monad
import Network.HTTP.Conduit
import Network (withSocketsDo)
import JsonInputReader
import GpsData
import BankData

-- Attention: the number of request per day is limited to 1000
-- It can be reached very rapidly!
findEstablishment place trn accuracy = withSocketsDo $
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