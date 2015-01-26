module GpsDataMining (getGpsEvents) where

import GpsData

{- GPS data mining -}

{- getGpsEvents will extract disjoint sublists of consecutive positions
   called "events" and verifying:
   * the duration of the event is larger than 5 minutes
   * the maximum speed during any one minute of the event is less than
     + 5 kph
     + half of the average speed the 5 minutes before and the 5 minutes
       after the event
   * the overall diameter of the event region is less than 1 kilometer
   
   I.e. we have very broad requirements which are devised to identify events:
   * during which the people are walking or staying still
   * which the people come to and depart from at a faster speed so that the
     region of the event is clearly identified
 -}
--getGpsEvents :: [Position] -> [[Position]]