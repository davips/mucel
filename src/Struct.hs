module Struct(Struct(Struct, sitems), Identifyable(idn), build, setTime, findMin, updatePairs, getPair, anda, setItem)
-- , wmapi, wmapt, wtoList, wmin, witem, wupdi, wdect, wupdt, soffset, smarked, wt)
where
import Debug; import Config
import Data.List
import Data.Function (on)
import qualified Data.Vector as V

-- data Node = Node {fstOrgId::Int, sndOrgId::Int, timeHeapNode::Float} deriving (Show, Eq)
-- instance Ord Node where compare = compare `on` timeHeapNode

data Struct a = Struct {sitems :: V.Vector a, stimes :: V.Vector Float, ssize :: Int, soffset :: Float, smin :: Float, smins :: V.Vector (Int, Int)} deriving Show
class Identifyable a where idn :: a -> Int

build :: (Identifyable a, Show a) => (a -> a -> Float) -> [a] -> Struct a
build ftime list = Struct items times n 0 m (V.map (\x -> quotRem x n) $ V.elemIndices m times)
  where n = length list
        m = V.minimum times
        items = V.fromList list
        times = V.fromList [if idn a < idn b then ftime a b else veryLargeFloat | a <- list, b <- list]


-- TODO otimizar substituindo varios maps por uma unica recursão com acumulador
setTime :: Struct a -> V.Vector (Int, Int) -> (a -> a -> Float) -> Struct a
setTime s@(Struct _ times n o m mins) pairs ftime = s {stimes = times', smin = m', smins = (V.map (\x -> quotRem x n) $ V.elemIndices m' times')}
-- setTime s@(Struct _ times n o m) pairs ftime = s {stimes = V.update times vecPairs, smin = min m (if null minlist then m else minimum minlist)}
-- setTime dt s@(Struct _ times n o) pairs ftime = s {stimes = V.unsafeUpdate times $ V.map (tup ftime) pairs} --, soffset = 0}
  where tup :: (Int, Int) -> (Int, Float)
        tup (a, b)       = let (itema, itemb) = getPair s (a, b)
                           in (address n a b, o + ftime itema itemb)
        vecPairs :: V.Vector (Int, Float)
        vecPairs         = V.map tup pairs'
        minlist          = V.map snd vecPairs
        pairs'           = V.fromList [if a<b then (a,b) else (b,a) | a <- [0..n-1], b <- uniqs, a /= b]
        uniqs            = nub $ fromTups $ V.toList pairs
        times'           = V.update times vecPairs
        m'               = V.minimum times'

address n a b = if a >= b then error "org a >= org b" else a * n + b

-- decrAll :: Float -> Struct a -> Struct a
-- decrAll dt s@(Struct _ _ _ o _) = s{soffset = dt + o}

findMin :: Struct t -> (Float, V.Vector (Int, Int))
findMin s@(Struct _ times n o m mins) = (m - o, mins)
-- findMin s@(Struct _ times n o m) = (m - o, V.map (\(x, _) -> quotRem x n) $ V.filter (\x -> snd x == m) $ V.indexed times)

updatePairs :: Identifyable a => Struct a -> ((a, a) -> (a, a)) -> V.Vector (Int, Int) -> Struct a
updatePairs s@(Struct items _ n _ _ _) f pairs = news
  where news = foldl f' s pairs
        f' s' (a, b) = let (a', b') = f $ getPair s' (a, b)
                           s'' = setItem s' a'
                       in setItem s'' b'

fromTups [] = []
fromTups ((x,y):xs) = x:y:fromTups xs


setItem :: Identifyable a => Struct a -> a -> Struct a
setItem s@(Struct items _ n _ _ _) item = s {sitems = V.update items $ V.fromList [(idn item, item)]}
-- setItem s@(Struct items _ n _) item = s {sitems = V.unsafeUpd items [(idn item, item)]}

getPair :: Struct t -> (Int, Int) -> (t, t)
getPair s@(Struct items _ _ _ _ _) (a, b) = ((V.!) items a, (V.!) items b)
-- getPair s@(Struct items _ n _ _) (a, b) = (V.unsafeIndex items a, V.unsafeIndex items b)

anda :: Float -> Struct a -> (Float -> a -> a) -> Struct a
anda dt s@(Struct items _ n o _ _) f = s {sitems = V.map (f dt) items, soffset = o + dt}

--
-- allPositions a n = let na = n * a
--                        nan = na + n
--                    in [na .. nan] ++ map (+ a) ([0, n .. na - n] ++ [nan, nan + n .. n * n - n])
--
--
-- -- setItems :: Struct a -> [a] -> Struct a
-- -- setItems s@(Struct items _ n _) xs = s {sitems = V.unsafeUpd items ()}
--
-- -- getTime (Struct _ times n o) a b = times V.! (a * n + b) - o
-- --
-- -- wmapt :: (a -> a -> Float -> Float) -> Struct a -> Struct a
-- -- wmapt f (Struct is ts o m) = Struct is (map f' ts) o m
-- --   where f' (Node ida idb t) = Node ida idb $ f (is V.! ida) (is V.! idb) (t - o) + o
-- --
-- -- wmin :: Struct t -> (Float, [(t, t)])
-- -- wmin (Struct is ts o _) = (minTime - o, lista)
-- --   where nodes = findMin ts
-- --         minTime = nodeTime $ head nodes
-- --         lista = map it nodes
-- --         it (Node ida idb _) = (is V.! ida, is V.! idb)
-- --
-- --
-- -- wupdt :: Identifyable a => (a -> a -> Float -> Float) -> [a] -> Struct a -> Struct a
-- -- wupdt f xs (Struct is ts o m) = Struct is (map f' selected ++ rest) o m
-- --   where f' (Node ida idb t) = Node ida idb $ f (is V.! ida) (is V.! idb) (t - o) + o
-- --         (selected, rest) = span crit ts
-- --         crit (Node a b _) = elem a ids || elem b ids
-- --         ids = map idn xs
-- --
-- -- wt :: Identifyable a => a -> a -> Struct a -> Node
-- -- wt a b (Struct _ ts _ _) = getNode (idn a) (idn b) ts
-- --
-- -- wtoList :: Struct a -> [a]
-- -- wtoList (Struct is _ _ m) = V.toList is
-- --
-- -- wmapi :: (a -> b) -> Struct a -> Struct b
-- -- wmapi f (Struct is ts o m) = Struct (V.map f is) ts o m
-- --
-- -- witem :: Identifyable a => a -> Struct a -> a
-- -- witem item (Struct is ts _ m) = is V.! (idn item)
-- --
-- -- wupdi :: Identifyable a => [a] -> Struct a -> Struct a
-- -- wupdi items (Struct is ts o m) = Struct is' ts o m
-- --   where is' = V.unsafeUpd is $ map tups items
-- --         tups item = (idn item, item)
