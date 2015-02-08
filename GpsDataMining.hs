module GpsDataMining (getGpsEvents , isFixed , event_diameter) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Time.Clock
import GpsData

{- GPS data mining -}

shortTime = 300
shortDistance = 100 -- depends on the accuracy of gps data

getGpsEvents :: [Position] -> [Event]
getGpsEvents = map (foldr1 merge) . groupBy interesect . unfoldr nextEvent

data Event = Event {
	event_all_positions :: [Position],
	event_location :: Location,
	event_begin :: UTCTime,
	event_end :: UTCTime,
	event_span :: NominalDiffTime,
	event_totalDistance :: Double,
	event_diameter :: Double
}
instance Show Event where
	show e =
		"Event { begin = " ++ show (event_begin e) ++
		", end = " ++ show (event_end e) ++
		", totalDistance = " ++ show (event_totalDistance e) ++
		", diameter = " ++ show (event_diameter e) ++ " }"

toEvent :: [Position] -> Maybe Event
toEvent [] = Nothing
toEvent pos  =
	let locs = map pos_location pos in
	let begin = pos_date $ head pos in
	let end = pos_date $ last pos in
	barycenter locs >>=
	\loc -> return $ Event {
		event_all_positions = pos,
		event_location = loc,
		event_begin = begin,
		event_end = end,
		event_span = end `diffUTCTime` begin,
		event_totalDistance = sTotalDistance locs,
		event_diameter = diameter locs
	}

isFixed :: Event -> Bool
isFixed = (==0) . event_diameter

interesect :: Event -> Event -> Bool
interesect e1 e2 = loc_distance (event_location e1) (event_location e2) < event_diameter e1 + event_diameter e2

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
		if (isEvent $ take sublist l) then sublist else 0
	in
	case sTakeSublistsWhile nextShortEvent input of
	0 -> nextEvent (tail input)
	nextEventSize ->
		let (event_pos , remaining) = splitAt nextEventSize input in
		do
			e <- toEvent event_pos
			return (e , drop 1 remaining)

-- the first argument is a function that given a sublist returns
-- a prefix size
-- what this function does is iteratively applying this function
-- on the tails of its second argument until the prefix size returned is 0
-- it will then return the prefix size obtained by merging all the prefixes
-- returned up to this point
sTakeSublistsWhile :: ([t] -> Int) -> [t] -> Int
sTakeSublistsWhile _ [] = 0
sTakeSublistsWhile f l =
	let n = f l in
	if n == 0 then 0 else max n $ sTakeSublistsWhile f $ tail l

-- nextShortTime returns a prefix size of Positions spanning at least the 5 next minutes
nextShortTime :: [Position] -> Int
nextShortTime [] = 0
nextShortTime (hd : tl) =
	sTakeUntil (\last -> pos_date last `diffUTCTime` pos_date hd >= shortTime) (hd : tl)

-- returns the shortest prefix size such that
-- the last prefix element verifies the condition
sTakeUntil :: (t -> Bool) -> [t] -> Int
sTakeUntil condition l =
	case findIndex condition l of
	Just i -> i + 1
	Nothing -> length l

isEvent pos =
	let loc = map pos_location pos in
	diameter loc <= max shortDistance (sTotalDistance loc * 120 / timeSpan pos)
