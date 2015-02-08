module GpsData (
	Position(Position),
	Location(Location),
	pos_location,
	pos_date,
	pos_distance,
	loc_distance,
	sameLocation,
	barycenter,
	diameter,
	loc_totalDistance,
	timeSpan
) where

import Data.Time
import Data.Function
import Geo.Computations

data Location = Location {
	loc_latitude :: Double,
	loc_longitude :: Double
} deriving (Show, Eq, Ord)

data Position = Position {
	pos_location :: Location,
	pos_date :: UTCTime
} deriving (Show, Eq)
instance Ord Position
	where compare x y =
		case compare (pos_date x) (pos_date y) of
		EQ -> compare (pos_location x) (pos_location y)
		r -> r
-- positions are ordered first by date then by latitude and longitude

toPoint :: Location -> Point
toPoint pos = pt (loc_latitude pos) (loc_longitude pos) Nothing Nothing

loc_distance :: Location -> Location -> Double
loc_distance = distance `on` toPoint

pos_distance :: Position -> Position -> Double
pos_distance = loc_distance `on` pos_location

sameLocation :: Location -> Location -> Bool
sameLocation l1 l2 =
	loc_latitude l1 == loc_latitude l2 &&
	loc_longitude l1 == loc_longitude l2

{- Functions on lists of locations -}

barycenter :: [Location] -> Maybe Location
barycenter [] = Nothing
barycenter l =
	let lat_sum = sum . map loc_latitude $ l in
	let lon_sum = sum . map loc_longitude $ l in
	let n = fromIntegral $ length l in
	Just $ Location (lat_sum / n) (lon_sum / n)
	
-- diameter returns the maximal distance between two Positions of the sublist
diameter :: [Location] -> Double
diameter [] = 0
diameter [_] = 0
diameter (hd : tl) = foldr (\pos tmpMax -> max tmpMax $ loc_distance hd pos) (diameter tl) tl

loc_totalDistance :: [Location] -> Double
loc_totalDistance [] = 0
loc_totalDistance [_] = 0
loc_totalDistance (hd1 : hd2 : tl) = loc_totalDistance (hd2 : tl) + loc_distance hd1 hd2

timeSpan :: [Position] -> Double
timeSpan [] = 0
timeSpan [_] = 0
timeSpan (hd : tl) = realToFrac $ pos_date (last tl) `diffUTCTime` pos_date hd

