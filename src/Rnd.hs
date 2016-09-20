module Rnd (randomlist) where
import           Data.List
import           System.Random

randomlist :: Float -> Float -> Int -> [Float]
randomlist min max seed = map (\x -> min + x * (max - min)) $ rndlist seed

rndlist :: Int -> [Float]
rndlist seed = unfoldr (Just . random) $ mkStdGen seed
