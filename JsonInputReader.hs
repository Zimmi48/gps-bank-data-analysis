{-# LANGUAGE OverloadedStrings  #-}
{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

module JsonInputReader (getJSONPositions , getEstablishments) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as BS
import GpsData
import Establishment

{- Functions to treat the JSON GPS data -}
-- JSON data is less cool to read but it contains information about accuracy

getJSONPositions :: BS.ByteString -> NominalDiffTime -> Double -> Day -> Day -> [Position]
-- the input data is already sorted
getJSONPositions input timeDiff minAccuracy =
        filter_track $ do
                point <- getPoints input
                let tMs = timestampMs point
                let t = take (length tMs - 3) tMs
                let pos = return $ Position
                                (toLocation
                                        (normalizeE7 $ latitudeE7 point)
                                        (normalizeE7 $ longitudeE7 point))
                                (addUTCTime timeDiff $ parseTimeOrError True defaultTimeLocale "%s" t)
                if accuracy point <= minAccuracy then pos else []

data JSONPoint = JSONPoint {
        timestampMs :: String,
        latitudeE7 :: Int,
        longitudeE7 :: Int,
        accuracy :: Double
} deriving (Show)

instance FromJSON JSONPoint where
    parseJSON (Object v) =
        JSONPoint <$>
        v .: "timestampMs" <*>
        v .: "latitudeE7"  <*>
        v .: "longitudeE7" <*>
        v .: "accuracy"
    parseJSON _ = mzero

data Gps = Gps { locations :: [JSONPoint] } deriving (Show)

instance FromJSON Gps where
    parseJSON (Object v) =
        Gps <$>
        v .: "locations"
    parseJSON _ = mzero

getPoints input = fromMaybe [] $ liftM locations (decode input :: Maybe Gps)

normalizeE7 :: Int -> Double
normalizeE7 x = fromIntegral x / 10000000

{- Functions to treat the JSON Google Places API response -}

data GooglePlaces = GooglePlaces { results :: [GooglePlace] } deriving (Show)

instance FromJSON GooglePlaces where
    parseJSON (Object v) =
        GooglePlaces <$>
        v .: "results"
    parseJSON _ = mzero

data GooglePlace = GooglePlace {
    geometry :: GoogleGeometry,
    name :: String,
    -- we could retreive types
    vicinity :: String
} deriving (Show)

instance FromJSON GooglePlace where
    parseJSON (Object v) =
        GooglePlace     <$>
        v .: "geometry" <*>
        v .: "name"     <*>
        v .: "vicinity"
    parseJSON _ = mzero

data GoogleGeometry = GoogleGeometry { location :: GoogleLocation } deriving (Show)

instance FromJSON GoogleGeometry where
    parseJSON (Object v) =
        GoogleGeometry <$>
        v .: "location"
    parseJSON _ = mzero

data GoogleLocation = GoogleLocation {
    lat :: Double,
    lng :: Double
} deriving (Show)

instance FromJSON GoogleLocation where
    parseJSON (Object v) =
        GoogleLocation <$>
        v .: "lat"     <*>
        v .: "lng"
    parseJSON _ = mzero

getGooglePlaces input = fromMaybe [] $ liftM results $ (decode input :: Maybe GooglePlaces)

getEstablishments :: BS.ByteString -> [Establishment]
getEstablishments input = getGooglePlaces input >>= (\pl ->
    let gLocation = location $ geometry pl in
    return $ Establishment
        (toLocation (lat gLocation) (lng gLocation))
        (name pl)
        (vicinity pl)
    )
