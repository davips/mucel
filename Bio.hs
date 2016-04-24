module Bio(world,drawCell,cellColor,cellFormat,Photo,Electro,Cell,World(World),moveAll,correct) where
import Graphics.Gloss
import Data.List
import Data.Ord
data Photo = None | Bulb | Sensor deriving (Eq, Show)
data Electro = Isolant | Wire | Battery | Motor Float Float deriving (Eq, Show)
data Cell = Cell {id::Int, x::Float, y::Float, vx::Float, vy::Float, photo::Photo, electro::Electro} deriving (Eq, Show)
--data Organism = Organism {cells::[Cell]}
data World = World [Cell]
-- data Collision = Collision {a::Cell, b::Cell, d::Float} deriving (Eq, Show)

mm=1
r=30 :: Float
r2=60  :: Float
smallest=0.000001
bulbs = filter (((==) Bulb) . photo)

cellColor None = greyN 0.5
cellColor Bulb = red
cellColor Sensor = blue
cellFormat _ Wire = circleSolid r
cellFormat _ Battery = thickCircle (r-8) 16
cellFormat electro (Motor dx dy) = pictures [circleSolid r, color (dark $ cellColor electro) $ arcSolid (ang - 10) (ang + 10) r]
     where ang = (atan2 dy dx) * 180 / 3.1415

drawCell (Cell _ x y _ _ photo electro) = translate x y $ color (cellColor photo) $ cellFormat photo electro

-- worstCollision::[Collision] -> Maybe Collision
-- worstCollision [] = error "WorstCollision requires at least one collision!"
-- worstCollision colls = minimumBy (comparing d) colls

move cell@(Cell _ x y vx vy _ _) = cell {x = x + vx, y = y + vy}
           
--          electrify (Cell x y vx vy photo None glued) = Cell (x+vx) (y+vy) vx vy photo electro glued
--          electrify (Cell x y vx vy photo Wire glued) = Cell (x+vx) (y+vy) vx vy photo electro glued
--          electrify (Cell x y vx vy photo Wire glued) = Cell (x+vx) (y+vy) vx vy photo electro glued
--          luminify (Cell x y vx vy photo Wire glued) = Cell (x+vx) (y+vy) vx vy photo electro glued

-- dists [] = []
-- dists (x:xs) = collisionsWith x xs : dists xs

collides cella cellb = dist (x cella) (y cella) (x cellb) (y cellb) < r2
-- collisionsWith _ [] = []
-- collisionsWith cella (cellb:cells) = (if d < r2 then [cellb] else []) ++ collisionsWith cella cells
--      where d = dist (x cella) (y cella) (x cellb) (y cellb)
-- collisionsWith cella (cellb:cells) = (if d < r2 then [Collision cella cellb d] else []) ++ collisionsWith cella cells
--      where d = dist (x cella) (y cella) (x cellb) (y cellb)

dist x1 y1 x2 y2 = sqrt((x1-x2)^2 + (y1-y2)^2)

distpl x0 y0 x1 y1 x2 y2 = abs(y21*x0 - x21*y0 + x2*y1 - y2*x1) / (sqrt (y21*y21 + x21*x21))
     where y21=y2 -y1
           x21=x2 -x1

world = [
--            Cell 1 200 (-10) (-1.7) 0.5 None Battery
--           , Cell 2 (-200) 10 2 0 None Wire
           Cell 3 47 (-77) 0 0 Bulb Battery
           , Cell 6 (-152) (-20) (mm) 0 Sensor (Motor 1 10)
--           , Cell 4 72 71 mm 0 None (Motor 10 10)
--           , Cell 5 (-2) 40 0 0 Bulb Wire
          , Cell 7 190 (-190) (-mm) (mm) Bulb (Motor 10 1)
     ]

moveAll cells = moveAll' cells cells
moveAll' [] _ = []
moveAll' (cell:cells) allCells = cell' : moveAll' cells (cell':cells)
     where cell' = correct (move cell) allCells

correct cell [] = cell
correct cella@(Cell ida x y dx dy ph el) (cellb@(Cell idb p q _ _ _ _):cells) 
     | ida == idb = correct cella cells
     | otherwise = correct (Cell ida sx sy sdx sdy ph el) cells
     where ((sx, sy), (sdx, sdy)) = if collides cella cellb then ((stopPoint x y dx dy p q), (0, 0)) else ((x, y), (dx, dy))

stopPoint x y dx dy p q = if dist x y sx sy < dist x y sx' sy' then (sx, sy) else (sx', sy')
     where
          m = dy / dx
          c = y - m*x
          (dA2,dB,dC,dRoot) = (2*(m*m + 1), 2*(m*c - m*q - p), q*q - r2*r2 + p*p - 2*c*q + c*c, sqrt (dB*dB - 2*dA2*dC))
          (sx, sy) = if abs dx < smallest then (x, (-eB + eRoot) / eA2) else ((-dB + dRoot) / dA2, m*sx + c) 
          (sx', sy') = if abs dx < smallest then (x, (-eB - eRoot) / eA2) else ((-dB - dRoot) / dA2, m*sx' + c)
          (eA2,eB,eC,eRoot) = (2, -2*q, q*q + (x-p)*(x-p) - r2*r2, sqrt(eB*eB - 2*eA2*eC))          

firstColl cella cells = minimumBy (comparing $ \cellb -> dist (x cella) (y cella) (x cellb) (y cellb)) $ riskyCells cella cells

riskyCells cell cells = filter (isNear cell) cells

isNear (Cell _ xi yi dx dy _ _) (Cell _ p q _ _ _ _) = d < d / 2 + r2 && dpl < r2 --checar se p,q está nas costas
     where (x, y) = (xi+dx/2, yi+dy/2)
           (xf, yf) = (xi+dx, yi+dy)
           d = dist x y p q
           dpl = distpl p q xi yi xf yf
           
           
           