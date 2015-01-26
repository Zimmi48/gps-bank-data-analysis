module OfxReader (Transaction , name , amount , trn_date , getDebits) where

import System.Locale
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time

{- Defining an insertion sort because it is quicker on nearly sorted arrays -}

insertion_sort :: Ord a => [a] -> [a]
insertion_sort = foldr insert []

{- Functions to treat the (SGML) OFX format -}

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
	getTransactionsSGML input >>=
	maybeToList . extractTrnDetails

getTransactionsSGML :: String -> [String]
getTransactionsSGML = between_separators "<STMTTRN>" "</STMTTRN>"

extractTrnDetails :: String -> Maybe Transaction
extractTrnDetails input =
	let contents = to_nodes_and_texts input in
	case foldr name_amount_date ([], [], []) contents of
		(name : _ , amount : _ , date : _) ->
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
to_nodes_and_texts :: String -> [(String , String)]
to_nodes_and_texts = even_list_to_couples . tail . splitOneOf "<>"

name_amount_date ("NAME", name) (names , amounts , dates) = (name : names , amounts , dates)
name_amount_date ("TRNAMT", amount) (names , amounts , dates) = (names , amount : amounts , dates)
name_amount_date ("DTPOSTED", date) (names , amounts , dates) = (names , amounts , date : dates)
name_amount_date _ acc = acc

even_list_to_couples :: [a] -> [(a,a)]
even_list_to_couples [] = []
even_list_to_couples (h :[]) = []
even_list_to_couples (h1 : h2 : tl) = (h1,h2) : (even_list_to_couples tl)