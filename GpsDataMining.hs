module GpsDataMining (getGpsEvents) where

import Data.List
import Data.Time.Clock
import Geo.Computations

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
getGpsEvents :: [Point] -> [[Point]]
getGpsEvents track =
	let candidates = map inits $ tails track in
	-- returns all candidate events grouped by initial point
	let candidates = map (dropWhile eventTooShort) candidates in
	-- first condition satisfied
	let candidates = map (filter smallSpread) candidates in
	-- third condition satisfied
	map last candidates

smallSpread :: [Point] -> Bool
smallSpread e = (< 1000) . distance (head e) (last e)

eventTooShort :: [Point] -> Bool
eventTooShort e = (< diff5Minutes) $ diffUTCTime (pntTime $ head e) (pntTime $ last e)

diff5Minutes = secondsToDiffTime $ 5 * 60