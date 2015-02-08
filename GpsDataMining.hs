module GpsDataMining (getGpsEvents , isFixed , event_diameter) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Time.Clock
import GpsData
import Sublist

{- GPS data mining -}

shortTime = 300
shortDistance = 100 -- depends on the accuracy of gps data

getGpsEvents :: [Position] -> [Event]
getGpsEvents = map (foldr1 merge) . groupBy interesect . unfoldr nextEvent

data Event = Event {
	event_all_positions :: [Position],
	event_position :: Position,
	event_span :: NominalDiffTime,
	event_totalDistance :: Double,
	event_diameter :: Double
}
instance Show Event where
	show e =
		let begin = pos_date $ event_position e in
		let end = addUTCTime (event_span e) begin in
		"Event { begin = " ++ show begin ++
		", end = " ++ show end ++
		", totalDistance = " ++ show (event_totalDistance e) ++
		", diameter = " ++ show (event_diameter e) ++ " }"

toEvent :: [Position] -> Maybe Event
toEvent [] = Nothing
toEvent l  =
	let Just loc = barycenter $ map pos_location l in
	let begin = pos_date $ head l in
	return $ Event {
		event_all_positions = l,
		event_position = Position loc begin,
		event_span = (pos_date $ last l) `diffUTCTime` begin,
		event_totalDistance = sTotalDistance $ fromList l, -- this is ugly
		event_diameter = diameter $ fromList l -- this is ugly
	}

isFixed :: Event -> Bool
isFixed = (==0) . event_diameter

interesect :: Event -> Event -> Bool
interesect e1 e2 = pos_distance (event_position e1) (event_position e2) < event_diameter e1 + event_diameter e2

merge :: Event -> Event -> Event
merge e = fromMaybe e . toEvent . on (++) event_all_positions e

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
