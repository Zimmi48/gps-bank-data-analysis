{-# LANGUAGE DeriveDataTypeable #-}
module GpsDataReader (getJSONPositions) where

import System.Locale
import Data.List
import Data.Time
import Text.JSON.Generic
import GpsData
				
{- Functions to treat the JSON GPS data -}
-- JSON data is less cool to read but it contains information about accuracy

getJSONPositions :: String -> Double -> Maybe (UTCTime , UTCTime) -> [Position]
-- the input data is already sorted
getJSONPositions input min_accuracy maybeBeginEnd =
	let track = do
		point <- getPoints input
		let tMs = timestampMs point
		let t = take (length tMs - 3) tMs
		if accuracy point <= min_accuracy then
			return $ Position
				(location
					(normalizeE7 $ latitudeE7 point)
					(normalizeE7 $ longitudeE7 point))
				(readTime defaultTimeLocale "%s" t)
		else []
	in
	reverse $ case maybeBeginEnd of
	Nothing -> track
	Just (begin , end) ->
		takeWhile ((>= begin) . pos_date) $
		dropWhile ((> end) . pos_date) track

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