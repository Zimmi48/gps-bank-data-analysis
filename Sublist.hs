module Sublist (
		Sublist, -- this is the type, not the data constructor
		sCons,
		sHead,
		sTail,
		sLast,
		sLength,
		sEmpty,
		fromList,
		toList,
		sSublist,
		sInits,
		sInitsRev,
		sTails
	) where
-- sLength is always correct by construction
-- (cannot be larger than length . sList)
import Data.List

data Sublist t = Sublist { sList :: [t] , sLength :: Int }

instance Show t => Show (Sublist t)
	where show l =
		case (sList l , sLength l) of
		([] , _) -> "[]"
		(_ ,  0) -> "[]"
		(hd : tl , n) -> "[" ++ show hd ++ showRemaining tl (n - 1)

showRemaining [] _ = "]"
showRemaining _  0 = "]"
showRemaining (hd : tl) n = "," ++ show hd ++ showRemaining tl (n - 1)

instance Eq t => Eq (Sublist t) where
	l1 == l2 =
		if sEmpty l1 then
			sEmpty l2
		else
			sHead l1 == sHead l2 && sTail l1 == sTail l2
	
-- HOW TO CREATE SUBLISTS

sCons :: t -> Sublist t -> Sublist t
sCons hd tl = Sublist (hd : sList tl) (sLength tl + 1)

fromList :: [t] -> Sublist t
fromList l = Sublist l $ length l

{- Usecase: sSublist l a b, where a <= b < length l,
   will return the sSublist l[a..b]
   Notes:
   * the indices start at 0
   * this function operates in linear time of a
-}
sSublist :: Sublist t -> Int -> Int -> Sublist t
sSublist l a b =
	if sEmpty l then
		Sublist [] 0
	else if a == 0 then
		Sublist (sList l) $ min (b + 1) (sLength l)
	else
		sSublist (sTail l) (a - 1) (b - 1)

-- HOW TO DESTRUCT SUBLISTS

sHead :: Sublist t -> t
sHead = head . sList

sTail :: Sublist t -> Sublist t
sTail l = Sublist (tail $ sList l) (sLength l - 1)

sLast :: Sublist t -> Maybe t
sLast l =
	case (sList l , sLength l) of
	([] , _) -> Nothing
	(_ ,  0) -> Nothing
	(hd : tl , n) -> Just $ maybe hd id $ sLast $  Sublist tl $ n - 1

sEmpty :: Sublist l -> Bool
sEmpty l = sLength l == 0 || null (sList l)

toList :: Sublist t -> [t]
toList l = take (sLength l) (sList l)

-- FUNCTIONS

-- returns all the suffixes of the argument (longest to shortest)
sTails :: Sublist t -> [Sublist t]
sTails l = zipWith Sublist (tails $ sList l) $ allSublistLengthsRev l

-- returns all the prefixes of the argument (longest to shortest)
sInitsRev :: Sublist t -> [Sublist t]
sInitsRev l = map (Sublist $ sList l) $ allSublistLengthsRev l

allSublistLengthsRev l = [sLength l, sLength l - 1 .. 0]

-- returns all the prefixes of the argument (shortest to longest)
sInits :: Sublist t -> [Sublist t]
sInits l = flip map [0 .. sLength l] $ Sublist $ sList l

