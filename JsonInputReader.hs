{-# LANGUAGE DeriveDataTypeable #-}
module JsonInputReader (getJSONPositions , getEstablishments) where

import System.Locale
import Data.List
import Data.Time
import Text.JSON.Generic
import GpsData

{- Functions to treat the JSON GPS data -}
-- JSON data is less cool to read but it contains information about accuracy

getJSONPositions :: String -> Double -> Day -> Day -> [Position]
-- the input data is already sorted
getJSONPositions input min_accuracy =
	filter_track $ do
		point <- getPoints input
		let tMs = timestampMs point
		let t = take (length tMs - 3) tMs
		let pos = return $ Position
				(toLocation
					(normalizeE7 $ latitudeE7 point)
					(normalizeE7 $ longitudeE7 point))
				(readTime defaultTimeLocale "%s" t)
		if accuracy point <= min_accuracy then pos else []

data JSONPoint = JSONPoint {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int,
	accuracy :: Double
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [JSONPoint] } deriving (Show, Data, Typeable)

getPoints :: String -> [JSONPoint]
getPoints input = locations (decodeJSON input :: Gps)

normalizeE7 :: Int -> Double
normalizeE7 x = fromIntegral x / 10000000

{- Functions to treat the JSON Google Places API response -}

data GooglePlaces = GooglePlaces { results :: [GooglePlace] } deriving (Show, Data, Typeable)

data GooglePlace = GooglePlace {
    geometry :: GoogleGeometry,
    name :: String,
    -- we could retreive types
    vicinity :: String
} deriving (Show, Data, Typeable)

data GoogleGeometry = GoogleGeometry { location :: GoogleLocation } deriving (Show, Data, Typeable)

data GoogleLocation = GoogleLocation {
    lat :: Double,
    lng :: Double
} deriving (Show, Data, Typeable)

getGooglePlaces input = results (decodeJSON input :: GooglePlaces)

data Establishment = Establishment {
    establishment_location :: Location,
    establishment_name :: String,
    establishment_address :: String
}
instance Show Establishment where
    show e = establishment_name e ++ " at " ++ establishment_address e

getEstablishments = map (\pl ->
    let gLocation = location $ geometry pl in
    Establishment
        (toLocation (lat gLocation) (lng gLocation))
        (name pl)
        (vicinity pl)
    ) . getGooglePlaces
