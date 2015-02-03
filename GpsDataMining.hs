module GpsDataMining () where

import Data.List
import Data.Time.Clock
import GpsData
import Sublist

{- GPS data mining -}

--getGpsEvents :: [Position] -> [Sublist Position]
--getGpsEvents track =

shortTime = 300

-- nextShortTime returns a sublist of Positions spanning at least the 5 next minutes
nextShortTime :: Sublist Position -> Sublist Position
nextShortTime l =
	if sEmpty l then fromList [] else
	let begin = pos_date $ sHead l in
	sTakeUntil (\last -> pos_date last `diffUTCTime` begin >= shortTime) l

-- diameter returns the maximal distance between two Positions of the sublist
diameter :: Sublist Position -> Double
diameter l =
	if sLength l <= 1 then 0 else
		let hd = sHead l in
		let tl = sTail l in
		sFoldr (\pos tmpMax -> max tmpMax $ pos_distance hd pos) (diameter tl) tl

shortDistance = 30 -- depends on the accuracy of gps data

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

-- nextEvent returns the next event (merging the successive 5 minutes events)
-- and the rest of the list
--nextEvent :: Sublist Position -> (Sublist Position , Sublist Position)
--nextEvent l = sTakeSublistsWhile nextShortTime isEvent