module BankData (Transaction(Transaction) , name , amount , trn_date) where

import Data.Time

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