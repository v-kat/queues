module Queue where

import qualified Data.List as List
import GHC.Exts(IsList(..))
--import Control.Exception(assert)
------------------------------------------------------------------
data StrictList a
  = StrictNil
  | StrictCons !a !(StrictList a)

instance Functor StrictList where
  fmap f = go where
    go StrictNil         = StrictNil
    go (StrictCons x xs) = StrictCons (f x) (go xs)
------------------------------------------------------------------
class Queue f where
  emptyQueue :: f a
  dequeue :: f a -> Maybe (a, f a)
  enqueue :: f a -> a -> f a

queue2List :: Queue f => f a -> [a]
queue2List xs = case dequeue xs of
  Nothing -> []
  Just (y, ys) -> y : queue2List ys

list2Queue :: Queue f => [a] -> f a
list2Queue = foldl (\acc a -> enqueue acc a) emptyQueue
------------------------------------------------------------------
newtype ListQueue a = ListQueue { unListQueue :: [a] }
  deriving (Show)

instance Functor ListQueue where
  fmap f (ListQueue list) = ListQueue $ fmap f list

instance Queue ListQueue where
  emptyQueue = ListQueue []
  dequeue (ListQueue [      ]) = Nothing
  dequeue (ListQueue (x : xs)) = Just (x, ListQueue xs)
  enqueue (ListQueue xs) x = ListQueue $ go xs where
    go [      ] = [x]
    go (y : ys) = y : go ys
------------------------------------------------------------------
data PairedLists a = PairedLists [a] [a]
  deriving (Show)

instance Functor PairedLists where
  fmap f (PairedLists l r) = PairedLists (fmap f l) (fmap f r)

instance Queue PairedLists where
  emptyQueue = PairedLists [] []
  dequeue (PairedLists [] ys) = case List.reverse ys of
    [] -> Nothing
    x : xs -> Just (x, PairedLists xs [])
  dequeue (PairedLists (x : xs) ys) = Just (x, PairedLists xs ys)
  enqueue (PairedLists xs ys) a = PairedLists xs (a : ys)
------------------------------------------------------------------
naiveRotate :: [a] -> [a] -> [a]
naiveRotate xs ys = xs <> List.reverse ys

-- | rotate L R A
-- Invariant:
-- |R| = |L| + 1
rotate :: [a] -> [a] -> [a] -> [a]
rotate [      ] [y     ] zs = y : zs
rotate (x : xs) (y : ys) zs = x : rotate xs ys (y : zs)
rotate _ _ _ = error "rotate: invariant violated"

-- | Dequeue is worst case O(log n)
data LazyPairedLists a = LazyPairedLists Int [a] Int [a]
  deriving (Show)

sizeLPLQueue :: LazyPairedLists a -> Int
sizeLPLQueue (LazyPairedLists n _ m _) = n + m

makeLPLQueue :: Int -> [a] -> Int -> [a] -> LazyPairedLists a
makeLPLQueue sl l sr r
  | sr <= sl = LazyPairedLists sl l sr r
  | sr == sl + 1 = LazyPairedLists (sl + sr) (rotate l r []) 0 []
  | otherwise = error "size invariant violated"

instance Queue LazyPairedLists where
  emptyQueue = LazyPairedLists 0 [] 0 []
  dequeue (LazyPairedLists 0 [] 0 []) = Nothing
  dequeue (LazyPairedLists n (x : xs) m ys) = Just (x, makeLPLQueue (n - 1) xs m ys)
  dequeue _ = error "dequeue failed"
  enqueue (LazyPairedLists n xs m ys) a = makeLPLQueue n xs (m + 1) (a : ys)

instance IsList (LazyPairedLists a) where
  type Item (LazyPairedLists a) = a
  fromList = foldl enqueue emptyQueue
  toList (LazyPairedLists _ xs _ ys) = naiveRotate xs ys
------------------------------------------------------------------
data RealTimeQueue a = RealTimeQueue [a] [a] [a]
  deriving (Eq, Show)

makeRTQueue :: [a] -> [a] -> [a] -> RealTimeQueue a
makeRTQueue as bs cs = case cs of
  [] -> let x = rotate as bs [] in RealTimeQueue x [] x
  _ : xs -> RealTimeQueue as bs xs

instance Queue RealTimeQueue where
  emptyQueue = RealTimeQueue [] [] []
  dequeue (RealTimeQueue (x : xs) ys zs) = Just (x, makeRTQueue xs ys zs)
  dequeue (RealTimeQueue [] _ _) = Nothing
  enqueue (RealTimeQueue xs ys zs) a = makeRTQueue xs (a : ys) zs
