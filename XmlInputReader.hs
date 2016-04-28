{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

module XmlInputReader (getDebits , getPositions) where

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Data.Char
import Data.Function
import BankData
import GpsData

{- Defining an insertion sort because it is quicker on nearly sorted arrays -}

insertion_sort :: Ord a => [a] -> [a]
insertion_sort = foldr insert []

{- Functions to treat the OFX format -}

-- the second argument may be a couple of begin / end date
getDebits :: String -> Maybe Day -> Maybe Day -> [Transaction]
getDebits input mbegin mend =
	let all_debits =
		filter ((> 0) . amount) $
		getTransactionsInner input >>=
		maybeToList . extractTrnDetails
	in
	let before_end =
		case mend of
		Nothing -> all_debits
		Just end -> filter ((<= end) . trn_date) all_debits
	in
	let after_begin =
		case mbegin of
		Nothing -> before_end
		Just begin -> filter ((>= begin) . trn_date) before_end
	in
	-- we reverse before sorting because the list will be nearly sorted then
	insertion_sort $ reverse after_begin

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
				trn_date = readTime defaultTimeLocale "%Y%m%d" $ take 8 date
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
name_amount_date ("NAME"   : name   : tl) = let (Nothing , amount , date) = name_amount_date tl in (Just $ clean name , amount , date)
name_amount_date ("TRNAMT" : amount : tl) = let (name , Nothing, date) = name_amount_date tl in (name , Just amount , date)
name_amount_date ("DTPOSTED" : date : tl) = let (name , amount , Nothing) = name_amount_date tl in (name , amount , Just date)
name_amount_date (_ : tl) = name_amount_date tl

clean name = unwords $ remove_repeating $ words name >>= \w ->
	if length w <= 2 && all isAlphaNum w then
		-- short words are too impredictable to be safely removed
		return w
	else if (head w == '&') then
		-- probably an unfiltered XML entity
		[]
	else
		groupBy ((&&) `on` isAlpha) w >>= \w -> case w of
		[] -> []
		-- only one letter in the middle of digits is rarely informative
		[_] -> []
		alpha -> return alpha

remove_repeating [] = []
remove_repeating (hd : tl) =
	let following = remove_repeating tl in
	if any (isPrefixOf hd) tl then following else (hd : following)

{- Functions to treat the KML format -}

-- the second argument may be a couple of begin / end date
getPositions :: String -> NominalDiffTime -> Day -> Day -> [Position]
getPositions input diffTime =
	let contents = to_nodes_and_texts input in
	let (dates , coords) = dates_coords contents in
	filter_track $
		zipWith (\date coord ->
			let longitude : latitude : _ = splitOn " " coord in
			Position
				(toLocation (read latitude) (read longitude))
				(addUTCTime diffTime $ readTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" date)
		) dates coords

dates_coords [] = ([], [])
dates_coords ("when"     : date  : tl) = let (dates , coords) = dates_coords tl in (date : dates , coords)
dates_coords ("gx:coord" : coord : tl) = let (dates , coords) = dates_coords tl in (dates , coord : coords)
dates_coords (_ : tl) = dates_coords tl
