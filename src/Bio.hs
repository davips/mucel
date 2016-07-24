module Bio(World, Organism(Wall, Uni, Multi), particleInfo, orgId, orgPos, orgVel, orgRad, updatePV, distOrgs, orgPosX, orgPosY, subOrgs) where
import Config; import Geometry
import Struct (Struct, Identifyable, idn)

type World = Struct Organism
data Organism = Uni {particleInfo::ParticleInfo}
              | Multi {particleInfo::ParticleInfo, aInfo::AngularInfo, subOrgs::[Organism]}
              | Wall {particleInfo::ParticleInfo} deriving (Show)
instance Identifyable Organism where idn = orgId

orgId = particleId . particleInfo
orgPos = particlePos . particleInfo
(orgPosX, orgPosY) = (vecX . particlePos . particleInfo, vecY . particlePos . particleInfo)
orgVel = particleVel . particleInfo
orgRad = particleRad . particleInfo
updatePV org p v = org {particleInfo = (particleInfo org) {particlePos = p, particleVel = v}}
distOrgs a b = dist (orgPos a) (orgPos b)

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
