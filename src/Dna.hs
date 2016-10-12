--{-# LANGUAGE FlexibleInstances #-}
module Dna (
--Fea, newdna
) where
--import qualified Data.Vector as V
import           Rnd
data Solid  =  Solid deriving (Show)
data Motor  =  Motor deriving (Show)
data Sensor = Sensor deriving (Show)
data Wire   =   Wire deriving (Show)

data F a = F a | None deriving (Show)
data V = V Float Float deriving (Show)
data Cell = Cell {soli :: F Solid, motr :: F Motor, sens :: F Sensor, wire :: F Wire, pos :: V, vel :: V} deriving (Show)

class Fe a where f :: Int -> F a
instance Fe Solid where f x = if x == 0 then None else F Solid
instance Fe Motor where f x = if x == 0 then None else F Motor
instance Fe Sensor where f x = if x == 0 then None else F Sensor
instance Fe Wire where f x = if x == 0 then None else F Wire

ff a b c d = Cell (f a) (f b) (f c) (f d) (V 0 0) (V 0 0)

-- feas = [Nothing, Just Motor, Just Sensor, Just Wire] --Solid
--nfeas = length feas
--dnasize = 91 -- 1+6+12+24+48

ncombs = 2^4
cells = [ff a b c d | a <- zu, b <- zu, c <- zu, d <- zu] where zu = [0, 1]

newdna :: Int -> Int -> [Cell]
newdna seed n = map (\idx -> cells !! round idx) $ take n $ randomlist 0 (fromIntegral ncombs - 1) seed

-- queue of cells between two layers
--segment n  = newdna

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
