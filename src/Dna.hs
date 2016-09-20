module Dna (newdna) where
import qualified Data.Vector as V
import           Rnd

data Feature = Sensor | Wire | Motor deriving (Show)
feas = [Sensor,Wire,Motor]
nfeas = length feas
dnasize = 91 -- 1+6+12+24+48

newdna ::Int->[Feature]
newdna seed = map (\idx -> feas !! round idx) $ take dnasize $ randomlist 0 (fromIntegral nfeas - 1) seed

-- data Photo = None | Bulb | Sensor deriving (Eq, Show)
-- data Electro = Isolant | Wire | Battery | Motor V deriving (Show)
-- --data Organism = Multi {cells::[Cell], pos::V, vel::V, angpos::V, angvel::V} | Cell {cellId::Int, pos::V, vel::V, photo::Photo, electro::Electro} deriving Show
-- cellColor None = greyN 0.5
-- cellColor Bulb = red
-- cellColor Sensor = blue
-- cellFormat _ Wire = circleSolid r
-- cellFormat _ Battery = thickCircle (3*r/4) (r/2)  -- radius thickness
-- cellFormat electro (Motor v) = pictures [circleSolid r, color (dark $ cellColor electro) $ arcSolid (degrees v - 10) (degrees v + 10) r]
-- drawCell (Cell _ (V x y) _ photo electro) = translate x y $ color (cellColor photo) $ cellFormat photo electro
-- move cell@(Cell _ p v _ _) = cell {pos = add p v}
