module GpsData (Position (Position) , pos_date) where

import Data.Time

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