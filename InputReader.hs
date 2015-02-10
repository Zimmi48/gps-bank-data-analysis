module InputReader (getDebits , getPositions) where

import System.Locale
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import BankData
import GpsData

{- Defining an insertion sort because it is quicker on nearly sorted arrays -}

insertion_sort :: Ord a => [a] -> [a]
insertion_sort = foldr insert []

{- Functions to treat the OFX format -}

-- the second argument may be a couple of begin / end date
getDebits :: String -> Maybe (UTCTime , UTCTime) -> [Transaction]
getDebits input maybeBeginEnd =
	let theFilter =
		case maybeBeginEnd of
		Nothing -> (\trn -> amount trn > 0)
		Just (begin , end) -> (\trn -> amount trn > 0 && trn_date trn >= begin && trn_date trn <= end) in
	-- we reverse before sorting because the list will be nearly sorted then
	insertion_sort . reverse .
	filter theFilter $
	getTransactionsInner input >>=
	maybeToList . extractTrnDetails

getTransactionsInner :: String -> [String]
getTransactionsInner = between_separators "<STMTTRN>" "</STMTTRN>"

extractTrnDetails :: String -> Maybe Transaction
extractTrnDetails input =
	let contents = to_nodes_and_texts input in
	case name_amount_date contents of
		(Just name , Just amount , Just date) ->
			Just $ Transaction {
				name = name ,
				amount = - (read amount) ,
				trn_date = readTime defaultTimeLocale "%Y%m%d%H%M%S%Q" date
			}
		_ -> Nothing

-- (between_separators sep1 sep2 input) returns only the substrings which
-- appear between separators sep1 and sep2
between_separators :: String -> String -> String -> [String]
between_separators sep1 sep2 = map (head . splitOn sep2) . tail . splitOn sep1
-- splitOn always returns a non-empty list so there is no problem with this definition

-- returns a list of couples of a node and its text content
to_nodes_and_texts :: String -> [String]
to_nodes_and_texts = splitOneOf "<>"

name_amount_date [] = (Nothing , Nothing , Nothing)
name_amount_date ("NAME"   : name   : tl) = let (Nothing , amount , date) = name_amount_date tl in (Just name , amount , date)
name_amount_date ("TRNAMT" : amount : tl) = let (name , Nothing, date) = name_amount_date tl in (name , Just amount , date)
name_amount_date ("DTPOSTED" : date : tl) = let (name , amount , Nothing) = name_amount_date tl in (name , amount , Just date)
name_amount_date (_ : tl) = name_amount_date tl

{- Functions to treat the KML format -}

-- the second argument may be a couple of begin / end date
getPositions :: String -> Maybe (UTCTime , UTCTime) -> [Position]
getPositions input maybeBeginEnd =
	let contents = to_nodes_and_texts input in
	let (dates , coords) = dates_coords contents in
	let track =
		zipWith (\date coord ->
			let longitude : latitude : _ = splitOn " " coord in
			Position (location (read latitude) (read longitude)) (readTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" date)
		) dates coords in
	reverse $ case maybeBeginEnd of
	Nothing -> track
	Just (begin , end) ->
		takeWhile ((>= begin) . pos_date) $
		dropWhile ((> end) . pos_date) track

dates_coords [] = ([], [])
dates_coords ("when"     : date  : tl) = let (dates , coords) = dates_coords tl in (date : dates , coords)
dates_coords ("gx:coord" : coord : tl) = let (dates , coords) = dates_coords tl in (dates , coord : coords)
dates_coords (_ : tl) = dates_coords tl