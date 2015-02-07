module GpsDataMining (getGpsEvents , isFixed) where

import Data.List
import Data.Time.Clock
import GpsData
import Sublist

{- GPS data mining -}

shortTime = 300
shortDistance = 30 -- depends on the accuracy of gps data

data Event = Event {
	event_positions :: [Position],
	event_begin :: UTCTime,
	event_end :: UTCTime,
	event_totalDistance :: Double,
	event_diameter :: Double,
	event_latitude :: Double,
	event_longitude :: Double
}
instance Show Event where
	show e =
		"Event { begin = " ++ show (event_begin e) ++
		", end = " ++ show (event_end e) ++
		", totalDistance = " ++ show (event_totalDistance e) ++
		", diameter = " ++ show (event_diameter e) ++ " }"

toEvent :: [Position] -> Maybe Event
toEvent [] = Nothing
toEvent l  =
	let Just (lat , lon) = barycenter l in
	return $ Event {
		event_positions = l,
		event_begin = pos_date $ head l,
		event_end = pos_date $ last l,
		event_totalDistance = sTotalDistance $ fromList l, -- this is ugly
		event_diameter = diameter $ fromList l, -- this is ugly
		event_latitude = lat,
		event_longitude = lon
	}

isFixed :: Event -> Bool
isFixed = (==0) . event_diameter

getGpsEvents :: [Position] -> [Event]
getGpsEvents = unfoldr nextEvent

-- nextEvent returns the next event (merging the successive 5 minutes events)
-- and the rest of the list minus the head
-- (because there must be a hole between events)
nextEvent :: [Position] -> Maybe (Event , [Position])
nextEvent [] = Nothing
nextEvent input =
	let nextShortEvent l =
		let sublist = nextShortTime l in
		if (isEvent $ sPrefix sublist l) then sublist else 0
	in
	case sTakeSublistsWhile nextShortEvent $ fromList input of
	0 -> nextEvent (tail input)
	nextEventSize ->
		let (event_pos , remaining) = splitAt nextEventSize input in
		do
			e <- toEvent event_pos
			return (e , drop 1 remaining)

-- nextShortTime returns a prefix size of Positions spanning at least the 5 next minutes
nextShortTime :: Sublist Position -> Int
nextShortTime l =
	if sEmpty l then 0 else
	let begin = pos_date $ sHead l in
	sTakeUntil (\last -> pos_date last `diffUTCTime` begin >= shortTime) l

-- diameter returns the maximal distance between two Positions of the sublist
diameter :: Sublist Position -> Double
diameter l =
	if sLength l <= 1 then 0 else
		let hd = sHead l in
		let tl = sTail l in
		sFoldr (\pos tmpMax -> max tmpMax $ pos_distance hd pos) (diameter tl) tl

sTotalDistance :: Sublist Position -> Double
sTotalDistance l =
	if sLength l <= 1 then 0 else
		let tl = sTail l in
		sTotalDistance tl + pos_distance (sHead l) (sHead tl)

timeSpan :: Sublist Position -> Double
timeSpan l =
	if sLength l <= 1 then 0 else
		case (sLast l) of
		(Just last) -> realToFrac $ pos_date last `diffUTCTime` pos_date (sHead l)
		_ -> 0

efficientTravelDistance l = sTotalDistance l * 120 / timeSpan l

isEvent l = diameter l <= max shortDistance (efficientTravelDistance l)
