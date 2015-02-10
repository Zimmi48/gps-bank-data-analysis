module GpsData (
	Position(Position),
	pos_location,
	pos_date,
	Location,
	location,
	Place,
	place,
	place_diameter,
	place_intersect,
	contains,
	place_merge,
	places_merge,
	sameLocation,
	diameter,
	totalDistance,
	timeSpan
) where

import Data.List
import Data.Time
import Data.Function
import Geo.Computations

type Location = Point
location lat lon = pt lat lon Nothing Nothing

data Place = Place {
	place_center :: Location,
	place_diameter :: Double,
	place_locs :: [Location]
} deriving (Eq)
instance Show Place
	where show pl =
		let c = place_center pl in
		"{diameter = " ++
		show (place_diameter pl) ++
		", center = (" ++
		show (pntLat c) ++
		"," ++
		show (pntLon c) ++
		")}"
place locs min_accuracy = foldr1 place_merge $ map (\c -> Place c min_accuracy [c]) locs

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

sameLocation :: Location -> Location -> Bool
sameLocation l1 l2 =
	pntLat l1 == pntLat l2 &&
	pntLon l1 == pntLon l2

inside :: Location -> Place -> Bool
loc `inside` pl  = 2 * distance loc (place_center pl) <= place_diameter pl

contains :: Place -> Place -> Bool
p1 `contains` p2 = all (`inside` p2) (place_locs p1)
	
place_intersect :: Place -> Place -> Bool
place_intersect p1 p2 =
	any (`inside` p2) (place_locs p1) ||
	any (`inside` p1) (place_locs p2)

place_merge :: Place -> Place -> Place
place_merge p1 p2 =
	if p1 `contains` p2 then p1
	else if p2 `contains` p1 then p2
	else
		let dist = (distance `on` place_center) p1 p2 in
		let new_diameter = dist + (place_diameter p1 + place_diameter p2) / 2 in
		Place
			(interpolate (place_center p1) (place_center p2) $
				1/2 + (place_diameter p2 - place_diameter p1) / (4 * dist)
			) new_diameter $ union (place_locs p1) (place_locs p2)

{- Functions on lists of locations -}
	
-- diameter returns the maximal distance between two Positions of the sublist
diameter :: [Location] -> Double
diameter [] = 0
diameter [_] = 0
diameter (hd : tl) = foldr (\pos tmpMax -> max tmpMax $ distance hd pos) (diameter tl) tl

{- Functions on lists of positions -}

timeSpan :: [Position] -> Double
timeSpan [] = 0
timeSpan [_] = 0
timeSpan (hd : tl) = realToFrac $ pos_date (last tl) `diffUTCTime` pos_date hd

{- Functions on list of places -}

-- repeat the operation until we reach a fixpoint
-- in practice, we reach that fixpoint in one step most of the time
-- so using this function can be a waste of computational ressources
places_merge :: [Place] -> [Place]
places_merge l =
	let iters = iterate places_merge_once l in
	let Just (True , pl) =
		find fst $
		zipWith (\x y -> (x == y , x)) iters $ drop 1 iters in
	pl

places_merge_once [] = []
places_merge_once (hd : tl) =
	let (here , elsewhere) = partition (place_intersect hd) (places_merge_once tl) in
	foldr place_merge hd here : elsewhere