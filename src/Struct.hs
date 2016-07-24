module Struct(Struct(Struct), Identifyable(idn), wbuild, wmapi, wmapt, wtoList, wmin, witem, wupdi, wdect) where
import PList
import qualified Data.Vector as V
data Struct a = Struct {sitems :: V.Vector a, stimes :: TimeList, offset :: Float}
class Identifyable a where idn :: a -> Int

wbuild :: Identifyable a => (a -> a -> Float) -> [a] -> Struct a
wbuild ftime list = Struct items times 0
  where items = V.fromList list
        times = build nodes
        nodes = [Node (idn a) (idn b) (ftime a b) | a <- list, b <- list, idn a < idn b]

wtoList :: Struct a -> [a]
wtoList (Struct is _ _) = V.toList is

wdect t s@(Struct _ _ o) = s{offset = o + t}

wmapi :: (a -> b) -> Struct a -> Struct b
wmapi f (Struct is ts o) = Struct (V.map f is) ts o

wmapt :: (a -> a -> Float -> Float) -> Struct a -> Struct a
wmapt f (Struct is ts o) = Struct is (map f' ts) o
  where f' (Node ida idb t) = Node ida idb $ f (V.unsafeIndex is ida) (V.unsafeIndex is idb) (t - o)

wmin :: Struct t -> (Float, t, t)
wmin (Struct items times o) = (minTime - o, V.unsafeIndex items idA, V.unsafeIndex items idB)
  where Node idA idB minTime = findMin times

witem :: Identifyable a => a -> Struct a -> a
witem item (Struct is ts _) = V.unsafeIndex is $ idn item

wupdi :: Identifyable a => a -> Struct a -> Struct a
wupdi item (Struct is ts o) = Struct is' ts o
  where is' = V.unsafeUpd is [(idn item, item)]

-- wmin' :: Struct t -> (Float, t, t)
-- wmin' (Struct items times) = (minTime, idA, idB)
--   where Node idA idB minTime = findMin times

-- wrefresh :: (a -> Int) -> (a -> a -> Float) -> Struct a -> [a] -> Struct a
-- wrefresh fid ftime (Struct items times) (x:xs) =
-- wrefresh fid ftime (Struct items times) [x] = map refresh times
--
-- refresh (Node ida idx _) = ftime (V.unsafeIndex items ida) (V.unsafeIndex items idx)
-- refresh (Node idx idb _) = ftime (V.unsafeIndex items idx) (V.unsafeIndex items idb)
-- refresh (Node idx idb _) = ftime (V.unsafeIndex items idx) (V.unsafeIndex items idb)
--   where idx = fid x
