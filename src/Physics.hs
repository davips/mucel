module Physics(timeToCollision, evolve, kinEnergy) where
import Config; import Debug; import Geometry; import Bio; import Struct
import Data.Maybe --; import Data.List

kinEnergy = map $ (^2) . mag . orgVel

timeToCollision :: Organism -> Organism -> Float
timeToCollision (Wall wallInfo) b = fromMaybe veryLargeFloat $ timeToHitWall (particleInfo b) wallInfo
timeToCollision a b@(Wall _) = timeToCollision b a
timeToCollision a b = fromMaybe veryLargeFloat $ timeToHit (particleInfo a) (particleInfo b)

evolve :: World -> Float -> World
evolve world dt
  | abs dt < verySmallFloat = world
  | hitTime > dt            = wdect dt $ anda dt world
  | hitTime == 0            = world'
  | otherwise               = evolve world' remTime
  where (hitTime, orga, orgb) = wmin world
        -- world'                = wupdt recalculate orgb' $ wupdt recalculate orga' $ worldAfterColl
        world'                = wmapt recalculate worldAfterColl
        remTime               = dt - hitTime
        worldAfterColl        = wupdi orgb' $ wupdi orga' $ anda hitTime world
        (orga', orgb')        = collide orga orgb

recalculate :: Organism -> Organism -> Float -> Float
recalculate a b t = timeToCollision a b

anda :: Float -> World -> World
anda t = wmapi $ walk t

walk :: Float -> Organism -> Organism
walk dt u@(Uni p) = u {particleInfo = move dt p}
walk dt (Multi p a os) = Multi p' a' os'
  where p' = move dt p
        a' = movea dt a
        os' = map (\x -> x {particleInfo = transroda mc ang $ particleInfo x}) os
        mc = particlePos p'
        ang = angPos a'
walk _ x = x

transroda :: Vec -> Float -> ParticleInfo -> ParticleInfo
transroda mc ang spi@(SubParticleInfo i _ d a _) = spi {particlePos = mc `add` r}
  where rot = ang + a
        r = V (d * cos rot) (d * sin rot)

collide :: Organism -> Organism -> (Organism, Organism)
collide wall@(Wall _) org
  |  wid == 0 || wid == 2 = (wall, updatePV org pos vel{vecY = -vecY vel})
  |  wid == 1 || wid == 3 = (wall, updatePV org pos vel{vecX = -vecX vel})
  where wid = orgId wall
        pos = orgPos org
        vel = orgVel org

collide orga orgb = (updatePV orga posA newVelA, updatePV orgb posB newVelB)
    where newVelA = ficaNoA `add` vaiProA
          newVelB = ficaNoB `add` vaiProB
          (ficaNoA, vaiProB) = decompoe posA posB velA
          (ficaNoB, vaiProA) = decompoe posB posA velB
          (posA, posB) = (orgPos orga, orgPos orgb)
          (velA, velB) = (orgVel orga, orgVel orgb)
