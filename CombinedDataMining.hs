{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

module CombinedDataMining (
    groupByDay,
    allPlacesAndEstablishments,
    SpendingEvent,
    getSpendingEvents
) where

import Control.Monad
import Data.Time
import Data.Time.Format
import Data.List
import Data.Maybe
import Data.Function
import GpsData
import BankData
import Establishment
import GooglePlaceRequest

groupByDay :: [Event] -> [Transaction] -> [( [Event] , [Transaction] )]
-- return lists of events and transactions for each day
-- events may be removed if they happened on a day with no transaction
-- transactions may be removed if they happened on a day with no GPS event
-- events may be duplicated if they spread over several days
groupByDay [] _ = []
groupByDay _ [] = []
groupByDay events (hdt : tlt) =
        ( todayEvents , hdt : todayTrns ) : groupByDay followingEvents followingTrns
        where
        today = trn_date hdt
        ( todayEvents , followingEvents ) = getDayEvents today events
        ( todayTrns   , followingTrns   ) = getDayTrns   today tlt

getDayEvents _ [] = ([] , [])
getDayEvents current (hd : tl)
        -- this event was in the past but there may be other events to consider
        | eEnd   < current = getDayEvents current tl
        -- this event starts in the future thus all subsequent events too
        | current < eBegin = ( [] , hd : tl )
        -- this event finishes now so it cannot appear in the following
        | eEnd  == current = ( hd : todayEvents , followingEvents )
        -- this event is today but also in subsequent days
        | otherwise = ( hd : todayEvents , hd : followingEvents )
                -- we give hd in argument because it may reappear in followingEvents
        where
        eBegin = utctDay . event_begin $ hd
        eEnd = utctDay . event_end $ hd
        ( todayEvents , followingEvents ) = getDayEvents current tl

getDayTrns current = span $ (== current) . trn_date

vendorsOfPlace :: Place -> [( [Event] , [Transaction] )] -> [String]
vendorsOfPlace pl = flip foldr [] $
        \(events , trns) acc ->
                if any ((== pl) . event_place) events then nub (map name trns) ++ acc else acc

allPlacesAndEstablishments :: String -> Int -> Double -> [( [Event] , [Transaction] )] -> [Place] -> IO [( Place, [(Establishment , String)] )]
allPlacesAndEstablishments apiKey maxRequests accuracy dayByDay places =
    liftM (zip places) $
    mapM aux places
    where
        aux place = placeEstablishments apiKey maxRequests accuracy place (vendorsOfPlace place dayByDay)

-- This new datatype will contain all the data has been extracted on one event
-- from all the sources
data SpendingEvent = SpendingEvent {
    spending_establishment :: Establishment,
    spending_amount :: Double,
    spending_begin :: UTCTime,
    spending_end :: UTCTime
} deriving (Eq)
instance Show SpendingEvent where
    show sp =
        "\n\t{Establishment = " ++
        show (spending_establishment sp) ++
        ";\n\t\t amount = " ++
        show (spending_amount sp) ++
        ";\n\t\t between " ++
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (spending_begin sp) ++
        " and " ++
        formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (spending_end sp) ++
        "}\n"

getSpendingEvents :: String -> Int -> Double -> [Event] -> [Place] -> [Transaction] -> IO [SpendingEvent]
getSpendingEvents apiKey maxRequests accuracy events places trns = do
    let dayByDay = groupByDay events trns
    pl_establs <- allPlacesAndEstablishments apiKey maxRequests accuracy dayByDay places
    return . concat $ map (getDaySpending pl_establs) dayByDay
    where
        getDaySpending _ (_ , []) = []
        getDaySpending pl_establs (events , trns) = events >>=
            -- let's consider one gps event at the time
            \e ->
            let pl = event_place e in
            -- for each event, we keep only the closest solution
            take 1 .
            sortBy (on compare $ (distance $ place_center pl) . establishment_location . spending_establishment) $
            do
                -- let's consider one possible establishment at the time
                (establ , keywords) <- fromMaybe [] . liftM snd $ find ((== pl) . fst) pl_establs
                -- let's consider one possible transaction at the time
                trn <- filter ((== keywords) . name) trns
                return $ SpendingEvent establ (amount trn) (event_begin e) (event_end e)
