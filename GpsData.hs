module GpsData (
	Position(Position),
	pos_location,
	pos_date,
	Location,
	toLocation,
    showLocation,
    distance,
	Place,
	place,
	place_diameter,
    place_center,
	Event,
	toEvent,
	event_place,
	event_begin,
	event_end,
	event_diameter,
	isFixed,
	inside,
	any_pos_in_place,
	place_intersect,
	contains,
	place_merge,
	places_merge,
	sameLocation,
	diameter,
	totalDistance,
	timeSpan,
	filter_track
) where

import Data.List
import Data.Time
import Data.Function
import Geo.Computations

type Location = Point
toLocation lat lon = pt lat lon Nothing Nothing
showLocation loc = show (pntLat loc) ++ "," ++ show (pntLon loc)

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
		showLocation c ++
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

data Event = Event {
	event_place :: Place,
	event_begin :: UTCTime,
	event_end :: UTCTime
}
instance Show Event where
	show e =
		"Event { from " ++ show (event_begin e) ++
		" to " ++ show (event_end e) ++
		" at " ++ show (event_place e) ++ " }"

event_diameter = place_diameter . event_place

toEvent :: Double -> [Position] -> Maybe Event
toEvent _ [] = Nothing
toEvent minimalDiameter pos =
	return $ Event {
		event_place = place (map pos_location pos) minimalDiameter,
		event_begin = pos_date $ head pos,
		event_end = pos_date $ last pos
	}

isFixed :: Double -> Event -> Bool
isFixed minimalDiameter = (==minimalDiameter) . event_diameter

{- Various functions on location and place -}

inside :: Location -> Place -> Bool
loc `inside` pl  = 2 * distance loc (place_center pl) <= place_diameter pl

contains :: Place -> Place -> Bool
p1 `contains` p2 = all (`inside` p2) (place_locs p1)

place_intersect :: Place -> Place -> Bool
place_intersect p1 p2 =
	any_pos_in_place (place_locs p1) p2 ||
	any_pos_in_place (place_locs p2) p1

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

any_pos_in_place :: [Location] -> Place -> Bool
any_pos_in_place pos pl = any (`inside` pl) pos

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

{- Some code that is common to XmlInputReader and JsonInputReader -}

filter_track track begin end = reverse $
	takeWhile ((>= begin) . utctDay . pos_date) $
	dropWhile ((> end) . utctDay . pos_date) track

