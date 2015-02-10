module GpsDataMining (getGpsEvents , getAllPlaces , placeFrequency , isFixed , event_diameter, event_place) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Time.Clock
import GpsData
import Sublist

{- GPS data mining -}

-- looks like this way of merging places is too large and almost everything end up in the same place
getAllPlaces = places_merge . map event_place

placeFrequency places events = map (\pl -> length . filter (contains pl . event_place) $ events) places

getGpsEvents :: Double -> NominalDiffTime -> [Position] -> [Event]
-- doing the merge or not does not seem to have a high impact on the number of distinct places
getGpsEvents minimalDiameter minimalDuration =
	map (foldr1 $ event_merge minimalDiameter) .
	groupBy (place_intersect `on` event_place) .
	unfoldr (nextEvent minimalDiameter minimalDuration)

data Event = Event {
	event_all_positions :: [Position],
	event_place :: Place,
	event_begin :: UTCTime,
	event_end :: UTCTime,
	event_span :: NominalDiffTime,
	event_totalDistance :: Double
}
instance Show Event where
	show e =
		"Event { begin = " ++ show (event_begin e) ++
		", end = " ++ show (event_end e) ++
		", totalDistance = " ++ show (event_totalDistance e) ++
		", diameter = " ++ show (event_diameter e) ++ " }"

event_diameter = place_diameter . event_place
	
toEvent :: Double -> [Position] -> Maybe Event
toEvent _ [] = Nothing
toEvent minimalDiameter pos =
	let locs = map pos_location pos in
	let begin = pos_date $ head pos in
	let end = pos_date $ last pos in
	return $ Event {
		event_all_positions = pos,
		event_place = place locs minimalDiameter,
		event_begin = begin,
		event_end = end,
		event_span = end `diffUTCTime` begin,
		event_totalDistance = totalDistance locs
	}

isFixed :: Double -> Event -> Bool
isFixed minimalDiameter = (==minimalDiameter) . event_diameter

-- This is quite a strange way of merging successive events because the
-- positions that separated the successive events are completely lost...
-- But we can argue that they were probably unacurrate.
event_merge :: Double -> Event -> Event -> Event
event_merge minimalDiameter e = fromMaybe e . toEvent minimalDiameter . on (++) event_all_positions e

-- nextEvent returns the next event (merging the successive 5 minutes events)
-- and the rest of the list minus the head
-- (because there must be a hole between events)
nextEvent :: Double -> NominalDiffTime -> [Position] -> Maybe (Event , [Position])
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
		do
			e <- toEvent minimalDiameter event_pos
			return (e , drop 1 remaining)

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
