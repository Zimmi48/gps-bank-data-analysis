import System.IO
import System.Locale
import System.Environment
import System.Exit
import Control.Monad
import Data.List.Split
import Data.Time
import Data.Time.Format

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

data Transaction = Transaction { name :: String , amount :: Double , date :: UTCTime } deriving (Show)
instance Eq Transaction where x == y = amount x == amount y
instance Ord Transaction where compare x y = compare (amount x) (amount y)

extractTrnDetails :: String -> Maybe Transaction
extractTrnDetails input =
	let contents = to_nodes_and_texts input in
	case foldr name_amount_date ([], [], []) contents of
		(name : _ , amount : _ , date : _) ->
			Just $ Transaction {
				name = name ,
				amount = - (read amount) ,
				date = readTime defaultTimeLocale "%Y%m%d%H%M%S%Q" date
			}
		_ -> Nothing

getDebits :: String -> [Transaction]
getDebits input = do
	transaction <- getTransactionsSGML input
	case extractTrnDetails transaction of
		Nothing -> []
		Just details ->
			if amount details > 0 then
				return details
			else
				[]
				
{- Functions to treat the JSON gps data -}

data Position = Position { latitude :: Int , longitude :: Int , date :: UTCTime } deriving (Show)
-- do we need accuracy too?

getJsonBlock :: String -> [String]
getJsonBlock = between_separators "{" "}"

--getPositions :: String -> [Position]
--getPositions

main = do
	args <- getArgs
	case args
		[bank_file , gps_file] ->
			inp_bank <- openFile bank_file ReadMode
			inp_gps <- openFile gps_file ReadMode
			bank <- hGetContents inp_bank
			gps <- hGetContents inp_gps
			let debits = getDebits bank
			--let max_trn = foldr max (head debits) (tail debits)
			--putStrLn . show $ max_trn
			putStrLn . show . head $ getJsonBlock gps
			hClose inp
		_ ->
			putStrLn "Incorrect number of arguments!"
			exitWith (ExitFailure 1)
