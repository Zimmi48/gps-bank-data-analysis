module GpsDataMining (getGpsEvents) where

import Data.List
import Data.Time.Clock
import Geo.Computations
import Sublist

{- GPS data mining -}

{- getGpsEvents will extract disjoint sublists of consecutive positions
   called "events" and verifying:
   1) the duration of the event is larger than 5 minutes
   2) the maximum speed during any one minute of the event is less than
     + 5 kph
     + half of the average speed the 5 minutes before and the 5 minutes
       after the event
   3) the overall diameter of the event region is less than 1 kilometer
   
   I.e. we have very broad requirements which are devised to identify events:
   * during which the people are walking or staying still
   * which the people come to and depart from at a faster speed so that the
     region of the event is clearly identified
 -}
-- let's start by just using condition 1 and 3
getGpsEvents :: [Point] -> [Sublist Point]
getGpsEvents track =
	let candidates = map sInits $ sTails $ fromList track in
	-- returns all candidate events grouped by initial point
	let candidates = map (dropWhile eventTooShort) candidates in
	-- first condition satisfied
	let candidates = map (filter smallSpread) candidates in
	-- third condition satisfied
	map last candidates

-- this is not optimal as the last element is recomputed each time
eventTooShort e = (< diff5Minutes) $ diffUTCTime (pntTime $ sHead e) (pntTime $ sLast e)

-- incorrect definition
smallSpread e = (< 1000) . distance (sHead e) (sLast e)
-- a correct definition would first compute a center point and the check the distance to this point

diff5Minutes = secondsToDiffTime $ 5 * 60