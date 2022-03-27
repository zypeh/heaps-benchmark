{-# LANGUAGE MagicHash #-}

module Main where

import Gauge
import Control.Monad
import System.Random

import qualified Z.Data.ListHeap as ListHeap
import qualified Z.Data.Heap as Heap

numbersGen :: IO [Int]
numbersGen = do replicateM 100000 (randomRIO (0, 100) :: IO Int)

main :: IO ()
main = do
    numbers <- numbersGen
    -- strings <- stringsGen
    defaultMain [
        bgroup "Numbers" [
            bench "Z.Data.ListHeap" $ whnf (ListHeap.toList . ListHeap.fromList) numbers,
            bench "Z.Data.Heap" $ whnf (Heap.toList . Heap.fromList) numbers
        ]
     ]