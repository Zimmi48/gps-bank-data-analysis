module GpsData (Position(Position) , pos_date , pos_distance , sameLocation) where

import Data.Time
import Data.Function
import Geo.Computations

data Position = Position {
	pos_latitude :: Double,
	pos_longitude :: Double,
	pos_date :: UTCTime
} deriving (Show, Eq)
instance Ord Position
	where compare x y =
		case compare (pos_date x) (pos_date y) of
		EQ -> compare (pos_latitude x , pos_longitude x) (pos_latitude y , pos_longitude y)
		r -> r
-- positions are ordered first by date then by latitude and longitude

toPoint :: Position -> Point
toPoint pos = pt (pos_latitude pos) (pos_longitude pos) Nothing (Just $ pos_date pos)

pos_distance :: Position -> Position -> Double
pos_distance = distance `on` toPoint

sameLocation :: Position -> Position -> Bool
sameLocation l1 l2 =
	pos_latitude l1 == pos_latitude l2 &&
	pos_longitude l1 == pos_longitude l2