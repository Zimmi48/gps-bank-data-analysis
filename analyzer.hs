import System.IO
import System.Environment
import System.Exit
--import Data.Function
--import Data.List
--import Geo.Computations
import OfxReader
import GpsDataReader

main = do
	args <- getArgs
	case args of
		[bank_file , gps_file] -> do
			inp_bank <- openFile bank_file ReadMode
			inp_gps <- openFile gps_file ReadMode
			bank <- hGetContents inp_bank
			gps <- hGetContents inp_gps
			let debits = getDebits bank
			let positions = getPositions gps
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
