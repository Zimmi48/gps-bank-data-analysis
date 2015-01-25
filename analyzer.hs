{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import System.Locale
import System.Environment
import System.Exit
import Control.Monad
import Data.Function
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Format
import Text.JSON.Generic

{- Defining an insertion sort because it is quicker on nearly sorted arrays -}

insertion_sort :: Ord a => [a] -> [a]
insertion_sort = foldr insert []

{- Functions to treat the (SGML) OFX format -}

-- (between_separators sep1 sep2 input) returns only the substrings which
-- appear between separators sep1 and sep2
between_separators :: String -> String -> String -> [String]
between_separators sep1 sep2 = map (head . splitOn sep2) . tail . splitOn sep1
-- splitOn always returns a non-empty list so there is no problem with this definition

getTransactionsSGML :: String -> [String]
getTransactionsSGML = between_separators "<STMTTRN>" "</STMTTRN>"

even_list_to_couples :: [a] -> [(a,a)]
even_list_to_couples [] = []
even_list_to_couples (h :[]) = []
even_list_to_couples (h1 : h2 : tl) = (h1,h2) : (even_list_to_couples tl)

-- returns a list of couples of a node and its text content
to_nodes_and_texts :: String -> [(String , String)]
to_nodes_and_texts = even_list_to_couples . tail . splitOneOf "<>"

name_amount_date ("NAME", name) (names , amounts , dates) = (name : names , amounts , dates)
name_amount_date ("TRNAMT", amount) (names , amounts , dates) = (names , amount : amounts , dates)
name_amount_date ("DTPOSTED", date) (names , amounts , dates) = (names , amounts , date : dates)
name_amount_date _ acc = acc

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

getDebits :: String -> [Transaction]
getDebits input =
	-- we reverse before sorting because the list will be nearly sorted then
	insertion_sort . reverse .
	filter (\trn -> amount trn > 0) $
	getTransactionsSGML input >>=
	maybeToList . extractTrnDetails
				
{- Functions to treat the JSON GPS data -}

data Point = Point {
	timestampMs :: String,
	latitudeE7 :: Int,
	longitudeE7 :: Int
} deriving (Show, Data, Typeable)
data Gps = Gps { locations :: [Point] } deriving (Show, Data, Typeable)

getPoints :: String -> [Point]
getPoints input = locations (decodeJSON input :: Gps)

data Position = Position {
	latitude :: Int,
	longitude :: Int,
	pos_date :: UTCTime
} deriving (Show, Eq)
instance Ord Position
	where compare x y =
		case compare (pos_date x) (pos_date y) of
			EQ -> compare (latitude x , longitude x) (latitude y , longitude y)
			r -> r
-- positions are ordered first by date then by latitude and longitude

getPositions :: String -> [Position]
-- the input data is already sorted
getPositions input = reverse $ do
	point <- getPoints input
	return $ Position {
		latitude = latitudeE7 point,
		longitude = longitudeE7 point,
		pos_date =
			let tMs = timestampMs point in
			let t = take (length tMs - 3) tMs in
			readTime defaultTimeLocale "%s" t
	}

{- GPS data mining -}

{- getGpsEvents will extract disjoint sublists of consecutive positions
   called "events" and verifying:
   * the duration of the event is larger than 5 minutes
   * the maximum speed during the event is less than
     + 5 kph
     + half of the average speed the 5 minutes before and the 5 minutes
       after the event
   * the overall diameter of the event region is less than 5 kilometers
   
   I.e. we have very broad requirements which are devised to identify events:
   * during which the people are walking or staying still
   * which the people come to and depart from at a faster speed so that the
     region of the event is clearly identified
 -}
getGpsEvents :: [Position] -> [[Position]]

main = do
	args <- getArgs
	case args of
		[bank_file , gps_file] -> do
			inp_bank <- openFile bank_file ReadMode
			inp_gps <- openFile gps_file ReadMode
			bank <- hGetContents inp_bank >>= getDebits
			gps <- hGetContents inp_gps
			let debits = getDebits bank
			let positions = getPositions gps
			-- test if debits are sorted
			--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map trn_date debits
			-- The answer was no but now transactions are sorted after extraction.
			-- test if positions are sorted already
			--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map pos_date positions
			-- The answer is yes.
			
			-- show the three biggest spending
			--putStrLn . show . take 3 . reverse $ sortBy (compare `on` amount) debits
			
			--putStrLn . show $ head positions
			
			hClose inp_bank
			hClose inp_gps
		_ -> do
			putStrLn "Incorrect number of arguments!"
			exitWith (ExitFailure 1)
