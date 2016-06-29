module Bio(World(World), initialWorld, evolve, cellRadius, orgPosition, orgSpeed, organisms) where
import Geometry

data Organism = Multi {orgId::Int, subOrgs::[Organism], orgPosition::Vec, orgSpeed::Vec, angpos::Vec, angvel::Vec} | Uni {cellId::Int, orgPosition::Vec, orgSpeed::Vec} deriving Show
data World = World {organisms::[Organism]}

cellRadius = 30
initialWorld = World [Uni 1 (V (-100) 0) (V 10 0), Uni 2 (V 100 0) (V (-10) 0)]

evolve :: World -> Float -> World
evolve (World orgs) timeStep = World $ map (move timeStep) orgs


move dt u@(Uni oid oldPos vel) = u {orgPosition = oldPos `add` (dt `sca` vel)}
move dt m@(Multi oid suborgs oldPos vel _ _) = m {orgPosition = oldPos `add` (dt `sca` vel)}

-- import System.Random
-- import Data.List
-- import qualified Data.Map as M
-- import Data.Maybe
-- import Debug.Trace
-- import Geometry
-- import Graphics.Gloss
-- import Data.List
-- import Data.Ord
-- import Data.Function (on)
-- data Photo = None | Bulb | Sensor deriving (Eq, Show)
-- data Electro = Isolant | Wire | Battery | Motor V deriving (Show)
-- data Organism = Multi {cellList::[Cell], pos::V, vel::V, angpos::V, angvel::V} | Cell {cellId::Int, pos::V, vel::V} deriving Show
-- --data Organism = Multi {cells::[Cell], pos::V, vel::V, angpos::V, angvel::V} | Cell {cellId::Int, pos::V, vel::V, photo::Photo, electro::Electro} deriving Show
-- data World = World [Cell]
-- -- data Collision = Collision {a::Cell, b::Cell, d::Float} deriving (Eq, Show)
-- instance Eq Cell where Cell ida _ _ _ _ == Cell idb _ _ _ _ = ida == idb
-- instance Ord Cell where compare = compare `on` cellId
--
-- w=800 :: Float
-- h=400 :: Float
-- mm=0.2 :: Float
-- bulbs = filter (((==) Bulb) . photo)
--
-- ball (Cell i p v _ _) = Ball i p v r
--
-- cellColor None = greyN 0.5
-- cellColor Bulb = red
-- cellColor Sensor = blue
-- cellFormat _ Wire = circleSolid r
-- cellFormat _ Battery = thickCircle (3*r/4) (r/2)  -- radius thickness
-- cellFormat electro (Motor v) = pictures [circleSolid r, color (dark $ cellColor electro) $ arcSolid (degrees v - 10) (degrees v + 10) r]
--
-- drawCell (Cell _ (V x y) _ photo electro) = translate x y $ color (cellColor photo) $ cellFormat photo electro
-- move cell@(Cell _ p v _ _) = cell {pos = add p v}
--
-- moveAll cells = moveAll' cells [] []
-- moveAll' [] ready transfs = M.elems $ M.fromListWith (\ca cb -> ca {vel = add (vel ca) (vel cb)}) (transfs ++ map idAliza ready)
-- moveAll' (cella@(Cell _ pa va _ _):cells) ready transfs = resto
--      where
--            others = cells ++ ready
--            cellaw = hitWall cella
--            collided = firstColl cellaw others
--            resto = moveAll' cells (cellares:ready) ((if collided == Nothing then [] else [idAliza cellTransf]) ++ transfs)
--            cellares = if collided == Nothing then move cellaw else cellaw'
--            cellb@(Cell _ pb _ _ _) = fromJust collided
--            cellaStopped = cellaw {pos = stopPoint pa va pb}
--            (cellaw', cellTransf) = speedTransfer cellaStopped cellb
--
-- idAliza::Cell -> (Int, Cell)
-- idAliza ce = (cellId ce, ce)
--
-- hitWall cella@(Cell _ pi@(V x y) v@(V vx vy) _ _)
--      | xf < -w + r || xf > w - r = cella {vel = V (-vx) vy}
--      | yf < -h + r || yf > h  - r = cella {vel = V vx (-vy)}
--      | otherwise = cella
--      where V xf yf = add pi v
--
-- speedTransfer cella@(Cell _ pa va _ _) cellb@(Cell _ pb _ _ _) = (cella {vel = fica}, cellb {vel = vai})
--   where (fica, vai) = decompoe pa pb va
--
-- firstColl :: Cell -> [Cell] -> Maybe Cell
-- firstColl (Cell _ pa va _ _) cells = if length res == 0 then Nothing
--                                            else Just (minimumBy (comparing $ \cellb -> dist pa (pos cellb)) res)
--      where res = risky pa va cells
--
-- risky p v cells = filter ((isNear p v) . pos) cells
--
--
-- world = world'
--      ++ map (\x@(Cell id p v _ _) -> x {cellId = id + length world', pos = sca 1.2 p}) world'
--                ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 2 * length world', pos = sca 0.3 p}) world'
--                ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 3 * length world', pos = sca 0.4 p}) world'
--                ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 4 * length world', pos = sca 0.5 p}) world'
--
-- r2' = r2 * 3
-- world' = [
--            Cell 0 (V 0 0) (V 0 0) None (Motor $ V (-1) (-1))
--           , Cell 1 (V 0 (-r2')) (V (-0.07) 0.05) None Battery
--           , Cell 2 (V (-r2') (-r2')) (V 0.02 0) None Wire
--           , Cell 3 (V (-r2') 0) (V (0.01) (-0.01)) Bulb Battery
--           , Cell 6 (V (r2') (-r2')) (V mm 0) Sensor (Motor $ V 1 10)
--           , Cell 4 (V (-r2') r2') (V mm 0) None (Motor (V 10 10))
--           , Cell 5 (V (0) (r2')) (V 0 0) Bulb Wire
--           , Cell 7 (V (r2') (r2')) (V (-mm) (mm)) Bulb (Motor $ V 10 1)
--      ]
--
-- randomlist :: Int -> StdGen -> [Int]
-- randomlist n = take n . unfoldr (Just . random)
--
-- nei = map (flip mod 2) $ randomlist 6 seed
