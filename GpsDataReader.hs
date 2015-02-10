{-# LANGUAGE DeriveDataTypeable #-}
module GpsDataReader (getPositions) where

import System.Locale
import Data.List
import Data.Time
import Text.JSON.Generic
import Geo.Computations
				
{- Functions to treat the JSON GPS data -}
-- This file is not used anymore because we rather use the simpler KML open format

getPositions :: String -> [Point]
-- the input data is already sorted
getPositions input = reverse $ do
	point <- getPoints input
	let tMs = timestampMs point
	let t = take (length tMs - 3) tMs
	let date = readTime defaultTimeLocale "%s" t
	return $ pt (normalizeE7 $ latitudeE7 point) (normalizeE7 $ longitudeE7 point) Nothing (Just date)

data JSONPoint = JSONPoint {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [JSONPoint] } deriving (Show, Data, Typeable)

getPoints :: String -> [JSONPoint]
getPoints input = locations (decodeJSON input :: Gps)

normalizeE7 :: Int -> Double
normalizeE7 x = fromIntegral x / 10000000