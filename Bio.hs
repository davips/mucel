module Bio(pos,vel,world,cellId,drawCell,cellColor,cellFormat,Photo,Electro,Cell,World(World),moveAll) where
import Data.Maybe
import Debug.Trace
import Geometry
import Graphics.Gloss
import Data.List
import Data.Ord
data Photo = None | Bulb | Sensor deriving (Eq, Show)
data Electro = Isolant | Wire | Battery | Motor V deriving (Show)
data Cell = Cell {cellId::Int, pos::V, vel::V, photo::Photo, electro::Electro} deriving Show
--data Organism = Organism {cells::[Cell]}
data World = World [Cell]
-- data Collision = Collision {a::Cell, b::Cell, d::Float} deriving (Eq, Show)

instance Eq Cell where Cell ida _ _ _ _ == Cell idb _ _ _ _ = ida == idb

w=680
h=375
mm=0.75 :: Float
bulbs = filter (((==) Bulb) . photo)

cellColor None = greyN 0.5
cellColor Bulb = red
cellColor Sensor = blue
cellFormat _ Wire = circleSolid r
cellFormat _ Battery = thickCircle (r-8) 16
cellFormat electro (Motor v) = pictures [circleSolid r, color (dark $ cellColor electro) $ arcSolid (degrees v - 10) (degrees v + 10) r]

drawCell (Cell _ (V x y) _ photo electro) = translate x y $ color (cellColor photo) $ cellFormat photo electro
move cell@(Cell _ p v _ _) = cell {pos = add p v}
           
moveAll cells = moveAll' cells []
moveAll' [] cells = reverse cells
moveAll' (cella@(Cell _ pa va _ _):cells) ready = resto
     where 
           others = cells ++ ready
           cellaw = hitWall cella
           collided = firstColl cellaw others
           resto = if collided == Nothing then moveAll' cells (cellares:ready)
                                          else moveAll' (trocaB cells) (cellares:(trocaB ready))
           cellares = if collided == Nothing then move cellaw else cellaw'
           cellb@(Cell _ pb _ _ _) = fromJust collided
           cellaStopped = cellaw {pos = stopPoint pa va pb}
           (cellaw', cellb') = speedTransfer cellaStopped cellb
           trocaB xs = let semB = delete cellb xs in
                           if length semB == length xs then xs else cellb' : semB

hitWall cella@(Cell _ pi@(V x y) v@(V vx vy) _ _)
     | xf < -w + r || xf > w - r = cella {vel = V (-vx) vy}
     | yf < -h + r || yf > h  - r = cella {vel = V vx (-vy)}
     | otherwise = cella
     where V xf yf = add pi v

speedTransfer cella@(Cell _ pa va _ _) cellb@(Cell _ pb vb _ _) = (cella {vel = fica}, cellb {vel = add vb vai})
     where (vai, fica) = decompoe pa pb va

firstColl :: Cell -> [Cell] -> Maybe Cell
firstColl (Cell _ pa va _ _) cells = if length res == 0 then Nothing
                                           else Just (minimumBy (comparing $ \cellb -> dist pa (pos cellb)) res)
     where res = risky pa va cells

risky p v cells = filter ((isNear p v) . pos) cells


world = world'
     ++ map (\x@(Cell id p v _ _) -> x {cellId = id + length world', pos = sca 1.2 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 2 * length world', pos = sca 1.3 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 3 * length world', pos = sca 1.4 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 4 * length world', pos = sca 1.5 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 5 * length world', pos = sca 1.6 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 6 * length world', pos = sca 1.7 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 7 * length world', pos = sca 1.8 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 8 * length world', pos = sca 1.9 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 9 * length world', pos = sca 2 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 10 * length world', pos = sca 2.1 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 11 * length world', pos = sca 2.2 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 12 * length world', pos = sca 2.3 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 13 * length world', pos = sca 2.4 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 14 * length world', pos = sca 2.5 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 15 * length world', pos = sca 2.6 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 16 * length world', pos = sca 2.7 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 17 * length world', pos = sca 2.8 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 18 * length world', pos = sca 2.9 p}) world'
               ++ map (\x@(Cell id p v _ _) -> x {cellId = id + 19 * length world', pos = sca 3 p}) world'

world' = [
           Cell 0 (V 0 0) (V 0 0) None (Motor $ V (-1) (-1))
          , Cell 1 (V 200 (-10)) (V (-1.7) 0.5) None Battery
          , Cell 2 (V (-200) 10) (V 2 0) None Wire
          , Cell 3 (V 87 $ -77) (V (0.1) (-0.1)) Bulb Battery
          , Cell 6 (V (-152) (-20)) (V mm 0) Sensor (Motor $ V 1 10)
          , Cell 4 (V 72 91) (V mm 0) None (Motor (V 10 10))
          , Cell 5 (V (-20) 67) (V 0 0) Bulb Wire
          , Cell 7 (V 190 $ -210) (V (-mm) (mm)) Bulb (Motor $ V 10 1)
     ]