module GpsDataMining (getGpsEventsAndPlaces , placeFrequency) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Time.Clock
import GpsData
import Sublist

{- GPS data mining -}

placeFrequency places events = map (\pl -> length . filter (contains pl . event_place) $ events) places

indexes :: [Int]
indexes = [0..]

place_merge_2 (pl1 , i1) (pl2 , i2) = (place_merge pl1 pl2 , i1 ++ i2)

-- This function returns the events and the places even if the latter can be
-- easily extracted from the former.
-- This is redundant information but yay laziness!
getGpsEventsAndPlaces :: Double -> NominalDiffTime -> [Position] -> ([Event] , [Place])
getGpsEventsAndPlaces minimalDiameter minimalDuration track =
	let raw_events = unfoldr (nextEvent minimalDiameter minimalDuration) track in
	let raw_events_pos = map (map pos_location) raw_events :: [[Location]] in
	let placeEvent (locs , i) places =
		let (here , elsewhere) = partition (any_pos_in_place locs . fst) places in
		foldr place_merge_2 (place locs minimalDiameter , [i]) here : elsewhere
	in
	-- It'd be interesting to know whether applying places_merge is useful or not
	let raw_events_pos_indexed = zip raw_events_pos indexes :: [([Location] , Int)] in
	let places = foldr placeEvent [] raw_events_pos_indexed in
	let place_of_event i =
		find ((i `elem`) . snd) places >>= \(pl,_) -> return pl
		:: Maybe Place
	in
	let merged_events =
		mapMaybe (toEvent minimalDiameter . concat . map fst) .
		groupBy ((==) `on` (place_of_event . snd)) $
		zip raw_events indexes
	in
	(merged_events , map fst places)

-- nextEvent returns the next event (merging the successive 5 minutes events)
-- and the rest of the list minus the head
-- (because there must be a hole between events)
nextEvent :: Double -> NominalDiffTime -> [Position] -> Maybe ([Position] , [Position])
nextEvent _ _ [] = Nothing
nextEvent minimalDiameter minimalDuration input =
	let nextShortEvent l =
		let sublist = nextShortTime minimalDuration l in
		if (isEvent minimalDiameter $ sPrefix sublist l) then sublist else 0
	in
	case sTakeSublistsWhile nextShortEvent $ fromList input of
	0 -> nextEvent minimalDiameter minimalDuration (tail input)
	nextEventSize ->
		let (event_pos , remaining) = splitAt nextEventSize input in
		return (event_pos , drop 1 remaining)

-- nextShortTime returns a prefix size of Positions spanning at least the 5 next minutes
nextShortTime :: NominalDiffTime -> Sublist Position -> Int
nextShortTime minimalDuration l =
	if sEmpty l then 0 else
	let begin = pos_date $ sHead l in
	sTakeUntil (\last -> pos_date last `diffUTCTime` begin >= minimalDuration) l

isEvent minimalDiameter sl =
	let pos = toList sl in
	let loc = map pos_location pos in
	let tot_dist = totalDistance loc in
	let duration = timeSpan pos in
	diameter loc <= max minimalDiameter (tot_dist * 120 / duration) &&
	-- speed limitation
	tot_dist <= 2*duration
