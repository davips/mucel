module Physics(timeToCollision, evolve, kinEnergy) where
import Config; import Debug; import Geometry; import Bio; import Struct
import Data.Maybe --; import Data.List
import Control.Arrow

kinEnergy :: [Organism] -> [Float]
kinEnergy = map $ (^2) . mag . orgVel

timeToCollision :: Organism -> Organism -> Float
timeToCollision (Wall wallInfo) b = fromMaybe veryLargeFloat $ timeToHitWall (particleInfo b) wallInfo
timeToCollision a b@(Wall _) = timeToCollision b a
timeToCollision a b = fromMaybe veryLargeFloat $ timeToHit (particleInfo a) (particleInfo b)

evolve world dt
  | dt == 0 = world
  | hitTime > dt            = wdect dt $ anda dt world
  | otherwise               = evolve world' (dt - hitTime)
  where (hitTime, orgs') = wmin world
        orgs = if length orgs' > 1
               then d2 ("empates", map (\(a,b)-> (orgId a, orgId b)) orgs') orgs'
               else orgs'
        wcollided       = foldl f (anda hitTime world) orgs
        world'          = wmapt recalculate wcollided

f :: Struct Organism -> (Organism, Organism) -> Struct Organism
f w (orga, orgb) = wupdi [orga', orgb'] w
    where (orga', orgb') = collide (witem orga w) (witem orgb w)

-- evolve world dt
--   | dt == 0 = world
--   | hitTime == 0            = wmapt recalculate $ wupdi [orga', orgb'] world
--   | hitTime > dt            = wdect dt $ anda dt world
--   | otherwise               = evolve (wdect hitTime $ anda hitTime world) (dt - hitTime)
--   where (hitTime, orgs) = wmin world
--         (orga, orgb)    = if length orgs > 1 then d2 (map (\(a,b)-> (orgId a, orgId b)) orgs) $ head orgs else head orgs
--         (orga', orgb')  = collide orga orgb

-- invert :: Float -> World -> World
-- invert t w = w'{smarked = t}
--   where w' = wmapt recalculate $ wmapi invertVel w

invertVel :: Organism -> Organism
invertVel org = updatePV org (orgPos org) ((-1) `sca` orgVel org)

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
collide org wall@(Wall _) = collide wall org
-- collide orga orgb = (orga', orgb')
collide orga orgb = (updatePV orga posA newVelA, updatePV orgb posB newVelB)
    where newVelA = ficaNoA `add` vaiProA
          newVelB = ficaNoB `add` vaiProB
          (ficaNoA, vaiProB) = decompoe posA posB velA
          (ficaNoB, vaiProA) = decompoe posB posA velB
          (posA, posB) = (orgPos orga, orgPos orgb)
          (velA, velB) = (orgVel orga, orgVel orgb)
          -- orga' = orga {particleInfo = move 0.001 (particleInfo $ updatePV orga posA newVelA)}
          -- orgb' = orgb {particleInfo = move 0.001 (particleInfo $ updatePV orgb posB newVelB)}
