module Bio(World(World), particleInfo, initialWorld, evolve, organisms) where
import Geometry; import Data.Maybe; import Debug.Trace; import Rnd; import Config; import Data.List
--import PQueue
--import PArray
import PList
import qualified Data.Vector as Ve

type Orgs = Ve.Vector Organism
data World = World {organisms::Orgs, collisionHeap::Horg}
data Organism = Uni {particleInfo::ParticleInfo}
              | Multi {particleInfo::ParticleInfo, aInfo::AngularInfo, subOrgs::[Organism]}
              | Wall {particleInfo::ParticleInfo} deriving (Show)

debug res = traceShow res res
orgId = particleId . particleInfo
orgPos = particlePos . particleInfo
orgVel = particleVel . particleInfo
updatePV org p v = org {particleInfo = (particleInfo org) {particlePos = p, particleVel = v}}
distOrgs a b = dist (orgPos a) (orgPos b)
initialWorld = World orgs heap
  where list = walls ++ cells
        walls = [Wall $ ParticleInfo 0 (V (1000) (1000)) (V 0 0) 0
              , Wall $ ParticleInfo 1 (V (-1000) (1000)) (V 0 0) 0
              , Wall $ ParticleInfo 2 (V (-1000) (-1000)) (V 0 0) 0
              , Wall $ ParticleInfo 3 (V (1000) (-1000)) (V 0 0) 0]
        rpos = randomlist 900 (-900)
        rvel = randomlist 400 (-400)
        rrad = randomlist 10 20
        ids = [4..totalCells + 3]
        rtup = zip6 ids (rpos 243) (rpos 745) (rvel 452) (rvel 245) (rrad 356)
        cells = [uni i x y vx vy r | (i, x, y, vx, vy, r) <- rtup]
        heap = build nodes
        nodes = [Node (orgId a) (orgId b) (timeToCollision a b) | a <- list, b <- list, orgId a < orgId b]
        orgs = Ve.fromList list

uni :: Int -> Float -> Float -> Float -> Float -> Float -> Organism
uni ido xpos ypos xvel yvel rad = Uni $ ParticleInfo ido (V xpos ypos) (V xvel yvel) rad

timeToCollision :: Organism -> Organism -> Float
timeToCollision (Wall particleInfoW) b = fromMaybe veryLargeFloat $ timeToHitWall (particleInfo b) particleInfoW
timeToCollision a b@(Wall _) = timeToCollision b a
timeToCollision a b = fromMaybe veryLargeFloat $ timeToHit (particleInfo a) (particleInfo b)

evolve :: World -> Float -> World
evolve world 0 = world
evolve (World orgs heap) timeStep = evolve newWorld remainingTime -- This recursion allows more fidelity in the animation.
  where newWorld = World newOrgs newHeap
        remainingTime = timeStep - dt
        newOrgs = Ve.map (updateOrg timeToFirstCollision dt orgIdA orgIdB posA posB velA velB) orgs
        newHeap = if timeStep <= timeToFirstCollision then decreaseAll dt heap
                  else updateAll (updateTime timeToFirstCollision newOrgs dt orgIdA orgIdB) heap
        dt = if timeToFirstCollision==0 then timeStep else min timeToFirstCollision timeStep
        Node orgIdA orgIdB timeToFirstCollision = findMin heap
        posA = orgPos $ moveUntil dt $ Ve.unsafeIndex orgs orgIdA
        posB = orgPos $ moveUntil dt $ Ve.unsafeIndex orgs orgIdB
        velA = orgVel $ Ve.unsafeIndex orgs orgIdA
        velB = orgVel $ Ve.unsafeIndex orgs orgIdB

updateOrg _ 0 _ _ _ _ _ _ org = org -- fazer batida nesse caso?
updateOrg _ _ _ _ _ _ _ _ w@(Wall _) = w
updateOrg timeToFirstCollision dt idA idB posA posB velA velB org
  | timeToFirstCollision == dt && orgId org == idB && (idA == 0 || idA == 2) = updatePV org posB velB {vecY = -vecY velB}
  | timeToFirstCollision == dt && orgId org == idB && (idA == 1 || idA == 3) = updatePV org posB velB {vecX = -vecX velB}
  | timeToFirstCollision == dt && orgId org == idA = updatePV org posA newVelA
  | timeToFirstCollision == dt && orgId org == idB = updatePV org posB newVelB
  | otherwise                                      = moveUntil dt org --existe essa cond?
    where newVelA = ficaNoA `add` vaiProA
          newVelB = ficaNoB `add` vaiProB
          (ficaNoA, vaiProB) = decompoe posA posB velA
          (ficaNoB, vaiProA) = decompoe posB posA velB

updateTime t1 _ t2 _ _ pair | t2 == 0 = pair
updateTime timeToFirstCollision orgs dt idAhit idBhit node@(Node idA idB time) = updateNode node newTime
  where newTime = if idA == idAhit || idA == idBhit || idB == idAhit || idB == idBhit
                  then timeToCollision (orgs `Ve.unsafeIndex` idA) (orgs `Ve.unsafeIndex` idB)
                  else time - dt

moveUntil :: Float -> Organism -> Organism
moveUntil 0 x = x
moveUntil dt u@(Uni p@(ParticleInfo oid oldPos vel _)) = u {particleInfo = p {particlePos = oldPos `add` (dt `sca` vel)}}
moveUntil dt m@(Multi p@(ParticleInfo oid oldPos vel _) _ _) = m {particleInfo = p {particlePos = oldPos `add` (dt `sca` vel)}}
moveUntil _ x = x



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
