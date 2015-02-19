{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

import System.IO
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Locale
import Data.Time
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Printf
import BankData
import GpsData
import XmlInputReader
import JsonInputReader
import GpsDataMining
import CombinedDataMining

{- Config file -}
apiKeyFile = do
    appDataDir <- getAppUserDataDirectory "gpsbank"
    createDirectoryIfMissing False appDataDir
    return $ appDataDir </> "apikey"

apiKey = do
    filename <- apiKeyFile
    fileExists <- doesFileExist filename
    if fileExists then
        readFile filename
    else do
        putStr "Please, provide a Google Places API key: "
        hFlush stdout
        key <- getLine
        writeFile filename key
        return key
        

{- Options -}
data Flag = Help | Duration Int | Accuracy Int | Requests Int | Json | Kml | Begin Day | End Day | TimeDifference Int deriving (Eq)
duration (Duration d) = Just d
duration _ = Nothing
accuracy (Accuracy a) = Just a
accuracy _ = Nothing
requests (Requests r) = Just r
requests _ = Nothing
begin (Begin d) = Just d
begin _ = Nothing
end (End d) = Just d
end _ = Nothing
timeDifference (TimeDifference t) = Just t
timeDifference _ = Nothing

options = [
	Option
		['h']
		["help"]
		(NoArg Help) $
		"Show this help message.\n",
	Option
		['d']
		["duration"]
		(ReqArg (Duration . read) "D") $
		"Set the minimal duration of events in seconds (default 300).\n",
	Option
		['a']
		["accuracy"]
		(ReqArg (Accuracy . read) "A") $
		"Set the minimal accuracy of GPS points in meters (default 40).\n" ++
		"Makes more sense if used in combination with the JSON option.\n",
	Option
		['r']
		["requests"]
		(ReqArg (Requests . read) "R") $
		"Set the maximal number of Google Places API requests per place (default 3).\n",
	Option
		['j']
		["json"]
		(NoArg Json) $
		"Specify that the input GPS file will be in Google's JSON format.\n",
	Option
		['k']
		["kml"]
		(NoArg Kml) $
		"Specify that the input GPS file will be in Google's KML format.\n" ++
		"This is the default format so using this option is not required.\n",
	Option
		['b']
		["begin"]
		(ReqArg (Begin . readTime defaultTimeLocale "%Y-%m-%d") "\"YYYY-MM-DD\"") $
		"Set the begin date for GPS and transaction data selection (bounds included).\n",
	Option
		['e']
		["end"]
		(ReqArg (End . readTime defaultTimeLocale "%Y-%m-%d") "\"YYYY-MM-DD\"") $
		"Set the end date for GPS and transaction data selection (bounds included).\n",
	Option
		['t']
		["timedifference"]
		(ReqArg (TimeDifference . read) "T") $
		"Set the time difference between your time zone and UTC in hours (default 0).\n"
	]

parseArgs = do
	argv <- getArgs
	name <- getProgName
	case parse argv of
		([]   , [gps,bank] , []) -> return ([] , gps , bank)
		(opts , [gps,bank] , []) ->
			if Help `elem` opts then
				help name
			else if Json `elem` opts && Kml `elem` opts then
				die name ["Error: option KML and JSON are incompatible.\n"]
			else
				return (opts , gps , bank)
		( _ , [gps,bank] , errs) -> die name (errs)
		(opts , _ , _) ->
			if Help `elem` opts then
				help name
			else
				die name ["Error: not the right number of arguments.\n"]
	where
		parse argv    = getOpt Permute options argv
		header name   = "Usage: " ++ name ++ " [options] gps_file bank_file\n"
		info name     = usageInfo (header name) options
		dump          = hPutStrLn stderr
		die name errs = dump (concat errs ++ info name) >> exitWith (ExitFailure 1)
		help name     = dump (info name)                >> exitWith ExitSuccess

main = do
	(opts, gps_file, bank_file) <- parseArgs

	-- if the file we need to read is in KML format we read it as a string
	gps  <- readFile gps_file
	-- if it is in JSON format we read it as a ByteString
	gpsJson <- C.readFile gps_file

	bank <- readFile bank_file

	let mbegin = listToMaybe $ mapMaybe begin opts
	let mend   = listToMaybe $ mapMaybe end   opts

	let debits = getDebits bank mbegin mend
	let begin = fromMaybe (trn_date $ head debits) mbegin
	let end   = fromMaybe (trn_date $ last debits) mend

	let minimalDiameter = fromIntegral .   fromMaybe 40 . listToMaybe $
		mapMaybe accuracy opts
	let minimalDuration = fromIntegral .  fromMaybe 300 . listToMaybe $
		mapMaybe duration opts
	let requestNb =                         fromMaybe 3 . listToMaybe $
		mapMaybe requests opts
	let timeDiff = fromIntegral . (*3600) . fromMaybe 0 . listToMaybe $
		mapMaybe timeDifference opts
	-- This timeDiff is a hack! We will remove it from the position dates
	-- (which are absolute) rather than adding it to the transaction dates
	-- because there is no proper support for time zones.

	-- using the first and last transaction to give bounds to GPS positions extractions
	let positions =
		if Json `elem` opts then
			getJSONPositions gpsJson timeDiff minimalDiameter begin end
		else
			getPositions gps timeDiff begin end

	let (events , places) = getGpsEventsAndPlaces minimalDiameter minimalDuration positions

	putStrLn $ "Between " ++ show begin ++ " and " ++ show end ++ ", you recorded:"
	printf "%d positions and\n" $ length positions

	putStr $ show (length debits) ++ " transactions at "
	putStrLn $ (show $ length $ nub $ map name debits) ++ " distinct vendors."

	printf "We found %d events.\n" $ length events
	--printf "Among these, %d are fixed.\n" (length . filter (isFixed minimalDiameter) $ events)

	let nonfixed_events = filter (not . isFixed minimalDiameter) events
	--printf
	--	"The others have an average diameter of %f meters.\n"
	--	( (sum $ map event_diameter nonfixed_events) / (fromIntegral $ length nonfixed_events) )
	printf "We identified %d distinct locations.\n" $ length places

	key <- apiKey
	spendingEvents <- getSpendingEvents key requestNb minimalDiameter events places debits
	print $ nub spendingEvents

	-- Various info

	-- Print 10 transactions
	--print $ take 10 debits

	-- All distinct vendor names
	--print $ nub $ map name debits

	-- Maximum number of transactions per day
	--let dayByDay = groupByDay events debits
	--print . maximum . map (length . snd) dayByDay

	-- Show the three biggest spending
	--putStrLn . show . take 3 . reverse $ sortBy (compare `on` amount) debits

	-- Most frequented places
	--print . take 10 . reverse . sortBy (compare `on` fst) $ zip (placeFrequency places events) places

	-- Time between two positions
	--let pntTimes = catMaybes $ map pntTime positions
	--print $ length $ filter (< 90) $ zipWith diffUTCTime (drop 1 pntTimes) pntTimes

	-- Test if debits are sorted
	--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map trn_date debits
	-- The answer was no but now transactions are sorted after extraction.
	-- Test if positions are sorted already
	--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map pntTime positions
	-- The answer is yes.
