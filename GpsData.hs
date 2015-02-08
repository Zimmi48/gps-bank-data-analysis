module GpsData (
	Position(Position),
	Location(Location),
	Place(Place),
	pos_location,
	pos_date,
	loc_distance,
	place_diameter,
	place_intersect,
	place_merge,
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

data Place = Place {
	place_center :: Location,
	place_diameter :: Double
} deriving (Show, Eq)

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

fromPoint :: Point -> Location
fromPoint pt = Location (pntLat pt) (pntLon pt)

loc_distance :: Location -> Location -> Double
loc_distance = distance `on` toPoint

sameLocation :: Location -> Location -> Bool
sameLocation l1 l2 =
	loc_latitude l1 == loc_latitude l2 &&
	loc_longitude l1 == loc_longitude l2

loc_interpolate :: Location -> Location -> Double -> Location
loc_interpolate l1 l2 w = fromPoint $ interpolate (toPoint l1) (toPoint l2) w

place_centers_distance = loc_distance `on` place_center

contains :: Place -> Place -> Bool
p1 `contains` p2 = 2 * place_centers_distance p1 p2 < place_diameter p1 - place_diameter p2
	
place_intersect :: Place -> Place -> Bool
place_intersect p1 p2 = 2 * place_centers_distance p1 p2 < place_diameter p1 + place_diameter p2

place_merge :: Place -> Place -> Place
place_merge p1 p2 =
	if p1 `contains` p2 then p1
	else if p2 `contains` p1 then p2
	else
		let dist = place_centers_distance p1 p2 in
		let new_diameter = dist + (place_diameter p1 + place_diameter p2) / 2 in
		Place
			(loc_interpolate (place_center p1) (place_center p2) $
				1/2 + (place_diameter p2 - place_diameter p1) / (4 * dist)
			) new_diameter

{- Functions on lists of locations -}

-- this code is probably incorrect, it should be improved with the use of addVector
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

{- Functions on lists of positions -}

timeSpan :: [Position] -> Double
timeSpan [] = 0
timeSpan [_] = 0
timeSpan (hd : tl) = realToFrac $ pos_date (last tl) `diffUTCTime` pos_date hd

