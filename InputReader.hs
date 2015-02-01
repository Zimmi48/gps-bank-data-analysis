module InputReader (Transaction , name , amount , trn_date , getDebits, getPositions) where

import System.Locale
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Geo.Computations

{- Defining an insertion sort because it is quicker on nearly sorted arrays -}

insertion_sort :: Ord a => [a] -> [a]
insertion_sort = foldr insert []

{- Functions to treat the OFX format -}

data Transaction = Transaction { name :: String , amount :: Double , trn_date :: UTCTime } deriving (Show, Eq)
instance Ord Transaction
	where compare x y =
		case compare (trn_date x) (trn_date y) of
			EQ ->
				case compare (amount x) (amount y) of
					EQ -> compare (name x) (name y)
					r -> r
			r -> r
-- transactions are ordered first by date then by amount and finally by name

getDebits :: String -> [Transaction]
getDebits input =
	-- we reverse before sorting because the list will be nearly sorted then
	insertion_sort . reverse .
	filter (\trn -> amount trn > 0) $
	getTransactionsInner input >>=
	maybeToList . extractTrnDetails

getTransactionsInner :: String -> [String]
getTransactionsInner = between_separators "<STMTTRN>" "</STMTTRN>"

extractTrnDetails :: String -> Maybe Transaction
extractTrnDetails input =
	let contents = to_nodes_and_texts input in
	case name_amount_date contents (Nothing, Nothing, Nothing) of
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

name_amount_date [] acc = acc
name_amount_date ("NAME"   : name   : tl) (Nothing , amount , date) = name_amount_date tl (Just name , amount , date)
name_amount_date ("TRNAMT" : amount : tl) (name , Nothing, date) = name_amount_date tl (name , Just amount , date)
name_amount_date ("DTPOSTED" : date : tl) (name , amount , Nothing) = name_amount_date tl (name , amount , Just date)
name_amount_date (_ : tl) acc = name_amount_date tl acc

{- Functions to treat the KML format -}

getPositions :: String -> [Point]
getPositions input =
	let contents = to_nodes_and_texts input in
	let (dates , coords) = dates_coords contents ([], []) in
	zipWith (\date coord ->
		let latitude : longitude : _ = splitOn " " coord in
		pt (read latitude) (read longitude) Nothing (Just $ readTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" date)
	) dates coords

dates_coords [] acc = acc
dates_coords ("when"     : date  : tl) (dates , coords) = dates_coords tl (date : dates , coords)
dates_coords ("gx:coord" : coord : tl) (dates , coords) = dates_coords tl (dates , coord : coords)
dates_coords (_ : tl) acc = dates_coords tl acc