{-# LANGUAGE DeriveDataTypeable #-}
module GpsDataReader (Position , latitude , longitude , pos_date , getPositions) where

import System.Locale
import Data.List
import Data.Time
import Text.JSON.Generic
				
{- Functions to treat the JSON GPS data -}

data Position = Position {
	latitude :: Int,
	longitude :: Int,
	pos_date :: UTCTime
} deriving (Show, Eq)
instance Ord Position
	where compare x y =
		case compare (pos_date x) (pos_date y) of
			EQ -> compare (latitude x , longitude x) (latitude y , longitude y)
			r -> r
-- positions are ordered first by date then by latitude and longitude

getPositions :: String -> [Position]
-- the input data is already sorted
getPositions input = reverse $ do
	point <- getPoints input
	return $ Position {
		latitude = latitudeE7 point,
		longitude = longitudeE7 point,
		pos_date =
			let tMs = timestampMs point in
			let t = take (length tMs - 3) tMs in
			readTime defaultTimeLocale "%s" t
	}

data Point = Point {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [Point] } deriving (Show, Data, Typeable)

getPoints :: String -> [Point]
getPoints input = locations (decodeJSON input :: Gps)