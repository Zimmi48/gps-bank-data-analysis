--module GpsDataMining (getGpsEvents) where

import Data.List
import Data.Time.Clock
import Geo.Computations
import Sublist

{- GPS data mining -}

--getGpsEvents :: [Point] -> [Sublist Point]
--getGpsEvents track =

shortTime = 300

-- nextShortTime returns a sublist of points spanning at least the 5 next minutes
nextShortTime :: Sublist Point -> Sublist Point
nextShortTime l =
	case pntTime $ sHead l of
	Nothing -> nextShortTime (sTail l)
	Just begin ->
		sTakeUntil
			(\last ->
				case pntTime last of
				Nothing -> False
				Just end -> end `diffUTCTime` begin >= shortTime
			) l

-- diameter returns the maximal distance between two points of the sublist
diameter :: Sublist Point -> Double
diameter l =
	if sEmpty l then 0 else
		let hd = sHead l in
		let tl = sTail l in
		sFoldr (\pnt tmpMax -> max tmpMax $ distance hd pnt) (diameter tl) tl

shortDistance = 30 -- depends on the accuracy of geo points

sTotalDistance :: Sublist Point -> Double
sTotalDistance l =
	if sLength l <= 1 then 0 else
	let tl = sTail l in
	sTotalDistance tl + distance (sHead l) (sHead tl)