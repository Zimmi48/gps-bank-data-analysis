{-# LANGUAGE DeriveDataTypeable #-}
module GpsDataReader (getPositions) where

import System.Locale
import Data.List
import Data.Time
import Data.GPS --we need version 0.3.1 at least
import Text.JSON.Generic

{- Functions to treat the JSON GPS data -}

getPositions :: String -> [Location (CoordinateDMS, UTCTime)]
-- the input data is already sorted
getPositions input = reverse $ do
	point <- getPoints input
	let tMs = timestampMs point
	let t = take (length tMs - 3) tMs
	let date = readTime defaultTimeLocale "%s" t
	let coords = degreePairToDMS (latitudeE7 point , longitudeE7 point)

data Point = Point {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [Point] } deriving (Show, Data, Typeable)

getPoints :: String -> [Point]
getPoints input = locations (decodeJSON input :: Gps)

cancelE7 :: Int -> Double
cancelE7 x = fromIntegral x / 10000000