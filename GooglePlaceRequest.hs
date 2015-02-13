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
findEstablishment :: String -> Double -> Place -> String -> IO (Maybe Establishment)
findEstablishment apiKey accuracy place vendor = withSocketsDo $
    simpleHttp url >>=
    \answer -> do
        let establ = listToMaybe $ getEstablishments answer
        --case establ of
        --  Nothing -> putStrLn "Establishment not found!"
        --  Just e  -> putStrLn $ "Found: " ++ show e
        return establ
    where
        url = base_url ++ location ++ radius ++ keyword ++ types ++ key
        base_url = "https://maps.googleapis.com/maps/api/place/search/json?"
        location = "location=" ++ showLocation (place_center place) ++ "&"
        radius   = "radius="   ++ show (max 100 $ 2*accuracy)       ++ "&"
        keyword  = "keyword="  ++ vendor                            ++ "&"
        types    = "types=establishment&"
        key      = "key=" ++ apiKey
        -- this can be changed

-- establishments remain associated with a vendor name for future reuse
placeEstablishments :: String -> Int -> Double -> Place -> [String] -> IO [( Establishment , String )]
placeEstablishments apiKey maxRequests accuracy place vendors =
    let tronc_vendors = take maxRequests vendors in
    liftM (catMaybes . map toMaybePair . flip zip tronc_vendors) .
    sequence $
    map (findEstablishment apiKey accuracy place) tronc_vendors
-- even if there are more vendors associated to one given place
-- we do not allow for a very large number of API requests per place

toMaybePair (Nothing , _) = Nothing
toMaybePair (Just x  , y) = Just (x , y)


