module Struct(Struct(Struct), Identifyable(idn), wbuild, wmapi, wmapt, wtoList, wmin, witem, wupdi, wdect, wupdt, soffset, smarked, wt) where
import PList; import Debug
import qualified Data.Vector as V
data Struct a = Struct {sitems :: V.Vector a, stimes :: TimeList, soffset :: Float, smarked :: Float} deriving Show
class Identifyable a where idn :: a -> Int

wbuild :: Identifyable a => (a -> a -> Float) -> [a] -> Struct a
wbuild ftime list = Struct items times 0 0
  where items = V.fromList list
        times = build nodes
        nodes = [Node (idn a) (idn b) (ftime a b) | a <- list, b <- list, idn a < idn b]

wmapt :: (a -> a -> Float -> Float) -> Struct a -> Struct a
wmapt f (Struct is ts o m) = Struct is (map f' ts) o m
  where f' (Node ida idb t) = Node ida idb $ f (is V.! ida) (is V.! idb) (t - o) + o

wmin :: Struct t -> (Float, [(t, t)])
wmin (Struct is ts o _) = (minTime - o, lista)
  where nodes = findMin ts
        minTime = nodeTime $ head nodes
        lista = map it nodes
        it (Node ida idb _) = (is V.! ida, is V.! idb)

wdect :: Float -> Struct a -> Struct a
wdect t s@(Struct _ _ o m) = s{soffset = t + o}

wupdt :: Identifyable a => (a -> a -> Float -> Float) -> [a] -> Struct a -> Struct a
wupdt f xs (Struct is ts o m) = Struct is (map f' selected ++ rest) o m
  where f' (Node ida idb t) = Node ida idb $ f (is V.! ida) (is V.! idb) (t - o) + o
        (selected, rest) = span crit ts
        crit (Node a b _) = elem a ids || elem b ids
        ids = map idn xs

wt :: Identifyable a => a -> a -> Struct a -> Node
wt a b (Struct _ ts _ _) = getNode (idn a) (idn b) ts

wtoList :: Struct a -> [a]
wtoList (Struct is _ _ m) = V.toList is

wmapi :: (a -> b) -> Struct a -> Struct b
wmapi f (Struct is ts o m) = Struct (V.map f is) ts o m

witem :: Identifyable a => a -> Struct a -> a
witem item (Struct is ts _ m) = is V.! (idn item)

wupdi :: Identifyable a => [a] -> Struct a -> Struct a
wupdi items (Struct is ts o m) = Struct is' ts o m
  where is' = V.unsafeUpd is $ map tups items
        tups item = (idn item, item)
