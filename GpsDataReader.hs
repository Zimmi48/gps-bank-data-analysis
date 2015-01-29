{-# LANGUAGE DeriveDataTypeable #-}
module GpsDataReader (getPositions) where

import System.Locale
import Data.List
import Data.Time
import Text.JSON.Generic
import GpsData
				
{- Functions to treat the JSON GPS data -}

getPositions :: String -> [Position]
-- the input data is already sorted
getPositions input = reverse $ do
	point <- getPoints input
	let tMs = timestampMs point
	let t = take (length tMs - 3) tMs
	let date = readTime defaultTimeLocale "%s" t
	return $ Position (latitudeE7 point) (longitudeE7 point) date

data Point = Point {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [Point] } deriving (Show, Data, Typeable)

getPoints :: String -> [Point]
getPoints input = locations (decodeJSON input :: Gps)