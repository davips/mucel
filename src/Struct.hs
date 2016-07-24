module Struct(Struct(Struct), Identifyable(idn), wbuild, wmapi, wmapt, wtoList, wmin, witem, wupdi, wdect, wupdt) where
import PList
import qualified Data.Vector as V
data Struct a = Struct {sitems :: V.Vector a, stimes :: TimeList, offset :: Float} deriving Show
class Identifyable a where idn :: a -> Int

wbuild :: Identifyable a => (a -> a -> Float) -> [a] -> Struct a
wbuild ftime list = Struct items times 0
  where items = V.fromList list
        times = build nodes
        nodes = [Node (idn a) (idn b) (ftime a b) | a <- list, b <- list, idn a < idn b]

wmapt :: (a -> a -> Float -> Float) -> Struct a -> Struct a
wmapt f (Struct is ts o) = Struct is (map f' ts) o
  where f' (Node ida idb t) = Node ida idb $ f (V.unsafeIndex is ida) (V.unsafeIndex is idb) (t - o) + o

wmin :: Struct t -> (Float, t, t)
wmin (Struct items times o) = (minTime - o, V.unsafeIndex items idA, V.unsafeIndex items idB)
  where Node idA idB minTime = findMin times

wdect :: Float -> Struct a -> Struct a
wdect t s@(Struct _ _ o) = s{offset = t + o}

wupdt :: Identifyable a => (a -> a -> Float -> Float) -> a -> Struct a -> Struct a
wupdt f x (Struct is ts o) = Struct is (map f' selected ++ rest) o
  where f' (Node ida idb t) = Node ida idb $ f (V.unsafeIndex is ida) (V.unsafeIndex is idb) (t - o) + o
        (selected, rest) = span crit ts
        crit (Node a b _) = a == idn x || b == idn x



wtoList :: Struct a -> [a]
wtoList (Struct is _ _) = V.toList is

wmapi :: (a -> b) -> Struct a -> Struct b
wmapi f (Struct is ts o) = Struct (V.map f is) ts o

witem :: Identifyable a => a -> Struct a -> a
witem item (Struct is ts _) = V.unsafeIndex is $ idn item

wupdi :: Identifyable a => a -> Struct a -> Struct a
wupdi item (Struct is ts o) = Struct is' ts o
  where is' = V.unsafeUpd is [(idn item, item)]
