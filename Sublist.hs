module Sublist (
		sCons,
		sHead,
		sTail,
		sLast,
		sLength,
		sEmpty,
		show,
		fromList,
		toList,
		sublistOfList,
		sInits,
		sInitsRev,
		sTails
	) where
-- encapsulation not working
import Data.List

data Sublist t = Sublist { sList :: [t] , sLength :: Int }
instance Show t => Show (Sublist t)
	where show l =
		case (sList l , sLength l) of
		([] , _) -> "[]"
		(_ ,  0) -> "[]"
		(hd : tl , n) -> "[" ++ show hd ++ showRemaining tl (n - 1)
instance Eq t => Eq (Sublist t) where
	l1 == l2 =
		if sEmpty l1 then
			sEmpty l2
		else
			sHead l1 == sHead l2 && sTail l1 == sTail l2

showRemaining [] _ = "]"
showRemaining _  0 = "]"
showRemaining (hd : tl) n = "," ++ show hd ++ showRemaining tl (n - 1)
	
-- HOW TO CREATE SUBLISTS

sCons :: t -> Sublist t -> Sublist t
sCons hd tl = Sublist (hd : sList tl) (sLength tl + 1)

fromList :: [t] -> Sublist t
fromList l = Sublist l $ length l

{- Usecase: sublistOfList l a b, where a <= b < length l,
   will return the sublist l[a..b]
   Notes:
   * the indices start at 0
   * this function does not check the precondition
   * this function operates in linear time of a
-}
sublistOfList :: [t] -> Int -> Int -> Sublist t
sublistOfList [] _ _ = Sublist [] 0
sublistOfList l 0 b = Sublist l (b + 1)
sublistOfList (hd : tl) a b = sublistOfList tl (a - 1) (b - 1)

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

sEmpty l = sLength l == 0 || sList l == []

toList :: Sublist t -> [t]
toList l = take (sLength l) (sList l)

-- FUNCTIONS

-- returns all the suffixes of the argument (longest to shortest)
sTails :: Sublist t -> [Sublist t]
sTails l = zipWith Sublist (tails $ sList l) $ allSublistLengthsRev l

-- returns all the prefixes of the argument (longest to shortest)
-- can generate the same answer multiple times if sLength is not the real length
sInitsRev :: Sublist t -> [Sublist t]
sInitsRev l = map (Sublist $ sList l) $ allSublistLengthsRev l

allSublistLengthsRev l = [sLength l, sLength l - 1 .. 0]

-- returns all the prefixes of the argument (shortest to longest)
-- can generate the same answer multiple times if sLength is not the real length
sInits :: Sublist t -> [Sublist t]
sInits l = flip map [0 .. sLength l] $ Sublist $ sList l

