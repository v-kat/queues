{-# language OverloadedStrings, AllowAmbiguousTypes #-}

module Main where

import Queue
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Functor

main :: IO ()
main = void tests

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

prop_list_queue_tripping :: forall f. Queue f => Property
prop_list_queue_tripping = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.enumBounded @Gen @Int)
  queue2List @f (list2Queue xs) === xs

tests :: IO Bool
tests =
  checkParallel $ Group "Test.Example"
  [ ("prop_reverse", prop_reverse)
  , ("prop_list_queue_tripping: ListQueue"  , prop_list_queue_tripping @ListQueue)
  , ("prop_list_queue_tripping: PairedLists", prop_list_queue_tripping @PairedLists)
  , ("prop_list_queue_tripping: LazyPairedLists", prop_list_queue_tripping @LazyPairedLists)
  , ("prop_list_queue_tripping: RealTimeQueue", prop_list_queue_tripping @RealTimeQueue)
  ]
