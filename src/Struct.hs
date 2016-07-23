module Struct(World(World), wbuild, wmap, wtoList) where
import PList
import qualified Data.Vector as V

type Items = V.Vector
data World a = World (Items a) TimeList

wbuild :: (a -> Int) -> (a -> a -> Float) -> [a] -> World a
wbuild fid ftime list = World items times
  where items = V.fromList list
        times = build nodes
        nodes = [Node (fid a) (fid b) (ftime a b) | a <- list, b <- list, fid a < fid b]

wmap :: (a -> b) -> World a -> World b
wmap f (World items times) = World (V.map f items) times

wtoList :: World a -> [a]
wtoList (World items times) = V.toList items
