
module Entrega2Lab7 where

import Prelude (Ord, (<=),(>), Show, Int, 
 error,(-), div, (!!), snd, otherwise, 
 (+), (<), (>=), (++), (.), (/), ($))


import List (length, drop, head, filter, sortBy, foldr)
import Bool
import Maybe
import qualified Heap as MnH
import qualified MaxHeap as MxH

data MedianHeap p q = MedianHeap Int Int (MxH.Heap p q) (MnH.Heap p q)

--Joana Maria Chaves Melo - 415604
------------------------------------------
filterLE :: Ord p => p -> [(p,q)]->[(p,q)]
filterLE p = filter (\(p',_) -> p' <= p)

filterGT :: Ord p => p -> [(p,q)]->[(p,q)]
filterGT p = filter (\(p',_) -> p' < p)

------------------------------------------
median :: Ord p => [(p, q)] -> Maybe (p,q)
median [] = Nothing
median xs = Just (p,q) where
	(p,q) = xs !? div (length xs) 2


(!?) :: [a] -> Int ->  a
[] !? _ = error "error"
(x: _) !? 0 = x
(_: xs ) !? k = xs !? (k - 1)
------------------------------------------

fromList :: Ord p => [(p,q)]-> MedianHeap p q
fromList xs = MedianHeap le gtl mxh mnh where
	Just (k,_) = median xs
		ls = filterLE k xs
		gt = filterGT k xs
		le = length ls
	 	gtl = length gt
		mxh = foldr (\kv acc -> MxH.insert acc kv) MxH.empty ls
		mnh = foldr (\kv acc -> MnH.insert acc kv) MnH.empty gt

--------------------------------------------

lookup :: Ord p => MedianHeap p q -> Maybe (p,q)
lookup (MedianHeap 0 0 _ _) = Nothing
lookup (MedianHeap lel gtl mxh mnh) = cond (lel >= gtl) l r where
	l = MxH.lookup mxh
	r = MnH.lookup mnh
--------------------------------------------

insert :: Ord k => MedianHeap k v -> (k,v) -> MedianHeap k v
insert (MedianHeap 0 0 mxh mnh) kv = MedianHeap 1 0 (MxH.insert mxh kv) mnh
insert h@(MedianHeap lel gtl mxh mnh) kv@(k,_) = rebalance $ nh where
	nh = cond (k<=k') l r
	l = MedianHeap (lel r 1) gtl (MxH.insert mxh kv) mnh
	r = MedianHeap lel (gtl + 1) mxh (MnH.insert mnh kv)
	Just (k',_) = lookup h
--------------------------------------------

rebalance :: Ord k => MedianHeap k v -> MedianHeap k v
	rebalance h@(MedianHeap lel gtl mxh mnh)
	| (lel < gtl) = MedianHeap (lel + 1) (gtl - 1) (MxH.insert mxh mnV) (MnH.pop mnh)
	| (lel > (gtl +1)) = MedianHeap (lel - 1) (gtl +1) (MxH.pop mxh) (MnH.insert mnh mxV)
	| otherwise = h 
	where
	 	Just mxV = MxH.lookup mxh
	 	Just mnV = MnH.lookup mnh
--------------------------------------------