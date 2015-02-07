import System.IO
import System.Environment
import System.Exit
import System.Locale
import Data.Time
--import Data.Function
import Data.List
import Data.Maybe
import Text.Printf
import BankData
import GpsData
import InputReader
import GpsDataMining

main = do
	args <- getArgs
	case args of
		[bank_file , gps_file , begin , end] -> do
			inp_bank <- openFile bank_file ReadMode
			inp_gps <- openFile gps_file ReadMode
			let beginDate = readTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" begin
			let endDate   = readTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" end
			bank <- hGetContents inp_bank
			gps  <- hGetContents inp_gps
			let debits = getDebits bank $ Just (beginDate , endDate)
			let positions = getPositions gps $ Just (beginDate , endDate)
			putStrLn $ "Between " ++ begin ++ " and " ++ end ++ ", you recorded:"
			printf "%d positions.\n" $ length positions
			let events = getGpsEvents positions
			printf "We found %d events.\n" $ length events
			printf "Among these, %d are fixed.\n" (length . filter isFixed $ events)
			let nonfixed_events = filter (not . isFixed) events
			printf
				"The others have an average diameter of %f meters.\n"
				( (sum $ map event_diameter nonfixed_events) / (fromIntegral $ length nonfixed_events) )
			
			--putStr $ show (length debits) ++ " transactions at "
			--putStrLn $ (show $ length $ nub $ map name debits) ++ " distinct vendors."
			--print $ nub $ map name debits
			
			-- time between two positions
			--let pntTimes = catMaybes $ map pntTime positions
			--print $ length $ filter (< 90) $ zipWith diffUTCTime (drop 1 pntTimes) pntTimes
			
			-- test if debits are sorted
			--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map trn_date debits
			-- The answer was no but now transactions are sorted after extraction.
			-- test if positions are sorted already
			--putStrLn . show . and . (\dates -> zipWith (<=) dates (drop 1 dates)) $ map pntTime positions
			-- The answer is yes.
			
			-- show the three biggest spending
			--putStrLn . show . take 3 . reverse $ sortBy (compare `on` amount) debits
			
			--putStrLn . show $ head positions
			
			hClose inp_bank
			hClose inp_gps
		_ -> do
			putStrLn "Incorrect number of arguments!"
			exitWith (ExitFailure 1)
