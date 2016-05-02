module Bio(world,drawCell,cellColor,cellFormat,Photo,Electro,Cell,World(World),moveAll) where
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

instance Eq Cell where 
     Cell ida _ _ _ _ == Cell idb _ _ _ _ = ida == idb
  
mm=1 :: Float
bulbs = filter (((==) Bulb) . photo)

cellColor None = greyN 0.5
cellColor Bulb = red
cellColor Sensor = blue
cellFormat _ Wire = circleSolid r
cellFormat _ Battery = thickCircle (r-8) 16
cellFormat electro (Motor v) = pictures [circleSolid r, color (dark $ cellColor electro) $ arcSolid (degrees v - 10) (degrees v + 10) r]

drawCell (Cell _ (V x y) _ photo electro) = translate x y $ color (cellColor photo) $ cellFormat photo electro
move cell@(Cell _ p v _ _) = cell {pos = add p v}
           
world = [
--            Cell 1 200 (-10) (-1.7) 0.5 None Battery
--           , Cell 2 (-200) 10 2 0 None Wire
           Cell 3 (V 47 $ -77) (V 0 0) Bulb Battery
           , Cell 6 (V (-152) (-20)) (V mm 0) Sensor (Motor $ V 1 10)
--           , Cell 4 72 71 mm 0 None (Motor 10 10)
--           , Cell 5 (-2) 40 0 0 Bulb Wire
          , Cell 7 (V 190 $ -190) (V (-mm) mm) Bulb (Motor $ V 10 1)
     ]

moveAll cells = moveAll' cells cells
moveAll' [] _ = []
moveAll' (cella@(Cell _ pa va _ _):cells) allCells = cella' : moveAll' cells (cella':cellb':cells')
     where cellb@(Cell _ pb _ _ _) = maybe (head cells) id res
           res = firstColl (pos cella) (vel cella) $ filter (/= cella) allCells
           noColl = res == Nothing
           cellaStopped = cella {pos = stopPoint pa va pb}
           (cella', cellb') = if noColl then (cella, cellb) else speedTransfer cellaStopped cellb
           cells' = filter (/= cellb) cells

speedTransfer cella@(Cell _ pa va _ _) cellb@(Cell _ pb vb _ _) = (cella {vel = fica}, cellb {vel = add vb vai})
     where (fica, vai) = decompoe pa pb va

firstColl :: V -> V -> [Cell] -> Maybe Cell
firstColl pa va cells = if length res == 0 then Nothing
                                           else Just (minimumBy (comparing $ \cellb -> dist pa (pos cellb)) res)
     where res = risky pa va cells

risky p v cells = filter ((isNear p v) . pos) cells

