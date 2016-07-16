module Rnd (randomlist) where
import System.Random
import Data.List

randomlist :: Float -> Float -> Int -> [Float]
randomlist max min seed = map (\x -> min + (max - min) * x) $ unfoldr (Just . random) $ mkStdGen seed
