{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import System.Locale
import System.Environment
import System.Exit
import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Format
import Text.JSON.Generic

{- Functions to treat the SGML OFX format -}

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
		case compare (amount x) (amount y) of
			EQ ->
				case compare (trn_date x) (trn_date y) of
					EQ -> compare (name x) (name y)
					r -> r
			r -> r
-- transactions are ordered first by amount then by date and finally by name

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
	filter (\trn -> amount trn > 0) $
	getTransactionsSGML input >>=
	maybeToList . extractTrnDetails
				
{- Functions to treat the JSON gps data -}

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
} deriving (Show)

getPositions :: String -> [Position]
getPositions input = do
	point <- getPoints input
	return $ Position {
		latitude = latitudeE7 point,
		longitude = longitudeE7 point,
		pos_date =
			let tMs = timestampMs point in
			let t = take (length tMs - 3) tMs in
			readTime defaultTimeLocale "%s" t
	}

main = do
	args <- getArgs
	case args of
		[bank_file , gps_file] -> do
			inp_bank <- openFile bank_file ReadMode
			inp_gps <- openFile gps_file ReadMode
			bank <- hGetContents inp_bank
			gps <- hGetContents inp_gps
			let debits = getDebits bank
			-- putStrLn . show . take 3 . reverse $ sort debits
			let positions = getPositions gps
			putStrLn . show $ head positions
			hClose inp_bank
			hClose inp_gps
		_ -> do
			putStrLn "Incorrect number of arguments!"
			exitWith (ExitFailure 1)
