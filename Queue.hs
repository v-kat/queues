module Queue where

import qualified Data.List as List
import Control.Exception(assert)

class Queue f where
  emptyQueue :: f a
  dequeue :: f a -> Maybe (a, f a)
  enqueue :: f a -> a -> f a
------------------------------------------------------------------
newtype ListQueue a = ListQueue { unListQueue :: [a] }
  deriving (Show)

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

instance Queue PairedLists where
  emptyQueue = PairedLists [] []
  dequeue (PairedLists [] ys) = case List.reverse ys of
    [] -> Nothing
    x : xs -> Just (x, PairedLists xs [])
  dequeue (PairedLists (x : xs) ys) = Just (x, PairedLists xs ys)
  enqueue (PairedLists xs ys) a = PairedLists xs (a : ys)

------------------------------------------------------------------
-- TODO write rotate
