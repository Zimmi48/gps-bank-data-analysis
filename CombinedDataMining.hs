module CombinedDataMining (groupByDays) where

import Data.Time
import GpsData
import BankData

groupByDays :: [Event] -> [Transaction] -> [( [Event] , [Transaction] )]
-- return lists of events and transactions for each day
-- events may be removed if they happened on a day with no transaction
-- transactions may be removed if they happened on a day with no GPS event
-- events may be duplicated if they spread over several days
groupByDays [] _ = []
groupByDays _ [] = []
groupByDays events (hdt : tlt) =
	( todayEvents , hdt : todayTrns ) : groupByDays followingEvents followingTrns
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

trnsOfPlace :: Place -> [( [Event] , [Transaction] )] -> [Transaction]
trnsOfPlace pl = flip foldr [] $
	\(events , trns) acc ->
		if any ((== pl) . event_place) events then trns ++ acc else acc


