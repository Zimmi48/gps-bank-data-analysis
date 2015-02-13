{- This Source Code Form is subject to the terms of the Mozilla Public License, v.2.0.
 - If a copy of the MPL was not distributed with this file,
 - You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

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
		sPrefix,
		sInits,
		sInitsRev,
		sTails,
		sTakeUntil,
		sFoldr,
		sSum,
		sProduct,
		sDrop,
		sTakeSublistsWhile
	) where
-- sLength is always correct by construction
-- (cannot be larger than length . sList)
import Data.List

-- should the length rather be a type parameter?
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

{- Usecase: sSublist l a n, where a + n <= length l,
   will return the sSublist l[a..(a + n - 1)]
   Notes:
   * the indices start at 0
   * this function operates in linear time of a
-}
sSublist :: Sublist t -> Int -> Int -> Sublist t
sSublist l a n =
	if sEmpty l then
		Sublist [] 0
	else if a == 0 then
		sPrefix n l
	else
		sSublist (sTail l) (a - 1) n

-- a prefix of a sublist can be consistently represented by an int
-- this function gives the sublist representation for this int.
sPrefix :: Int -> Sublist t -> Sublist t
sPrefix n l = Sublist (sList l) $ min n (sLength l)

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

-- returns the shortest prefix size such that
-- the last prefix element verifies the condition
sTakeUntil :: (t -> Bool) -> Sublist t -> Int
sTakeUntil condition l =
	let sL = sList l in
	case findIndex condition sL of
	Just i -> i + 1
	Nothing -> sLength l

sFoldr :: (t -> s -> s) -> s -> Sublist t -> s
sFoldr f init l =
	if sEmpty l then init else
		sHead l `f` sFoldr f init (sTail l)

sSum = sFoldr (+) 0
sProduct = sFoldr (*) 1

sDrop n l =
	if n <= 0 then l else Sublist (drop n $ sList l) (max 0 $ sLength l - n)

-- the first argument is a function that given a sublist returns
-- a prefix size
-- what this function does is iteratively applying this function
-- on the tails of its second argument until the prefix size returned is 0
-- it will then return the prefix size obtained by merging all the prefixes
-- returned up to this point
-- precondition not checked : f l <= sLength l for all l
sTakeSublistsWhile :: (Sublist t -> Int) -> Sublist t -> Int
sTakeSublistsWhile f l =
	let n = f l in
	if n == 0 then 0 else max n $ sTakeSublistsWhile f $ sTail l
