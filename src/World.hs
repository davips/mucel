module World (initialWorld) where
import Config; import Geometry; import Bio; import Struct; import Rnd; import Debug
import Physics (timeToCollision)
import Data.List

w2 = width / 2

initialWorld = wbuild timeToCollision (walls ++ cells ++ multicells)
  where walls = [Wall $ ParticleInfo 0 (V w2 w2) (V 0 0) 0
              , Wall $ ParticleInfo 1 (V (-w2) w2) (V 0 0) 0
              , Wall $ ParticleInfo 2 (V (-w2) (-w2)) (V 0 0) 0
              , Wall $ ParticleInfo 3 (V w2 (-w2)) (V 0 0) 0]
        cells = [uni i x y vx vy r | (i, x, y, vx, vy, r) <- rtup]
        multicells = [multi i x y vx vy a n minRad (seed + 324 * i) | (i, x, y, vx, vy, a, n) <- rtupm]
        rtup = zip6 [4..totalCells + 3] (rpos $ seed + 243) (rpos $ seed + 745) (rvel $ seed + 452) (rvel $ seed + 245) (rrad $ seed + 356)
        rtupm = zip7 [totalCells + 4..(totalCells + 3) + totalMultis] (rpos $ seed + 43) (rpos $ seed + 75) (rvel $ seed + 42) (rvel $ seed + 24) (rang $ seed + 56) (rn $ seed + 36)
        rpos = randomlist minPos maxPos
        rvel = randomlist minVel maxVel
        rrad = randomlist minRad maxRad
        rang = randomlist minChainAng maxChainAng
        rn seed = map round $ randomlist minSub maxSub seed

uni :: Int -> Float -> Float -> Float -> Float -> Float -> Organism
uni ido xpos ypos xvel yvel rad = Uni $ ParticleInfo ido (V xpos ypos) (V xvel yvel) rad

uni2subuni :: Vec -> Organism -> Organism
uni2subuni mc org = Uni $ SubParticleInfo ido p (dist p mc) (atan2 dy dx) r
  where V dx dy = p `sub` mc
        ParticleInfo ido p _ r = particleInfo org

multi :: Int -> Float -> Float -> Float -> Float -> Float -> Int -> Float -> Int -> Organism
multi ido xhead yhead xvel yvel avel n cellRad seed = Multi pinfo ainfo orgs
  where pinfo = ParticleInfo ido mc (V xvel yvel) rad
        ainfo = AngularInfo 0 avel
        orgs = map (uni2subuni mc) $ chain 0 (ido + 1) cellRad xhead yhead angs
        angs = take n $ randomlist minChainAng maxChainAng seed
        mc = massCenter posis
        rad = maxDist posis / 2 + cellRad
        posis = map orgPos orgs

chain :: Float -> Int -> Float -> Float -> Float -> [Float] -> [Organism]
chain _ _ _ _ _ []  = []
chain ang0 ido rad x y (ang:angs) = uni ido x y 0 0 rad : chain (ang + ang0) (ido + 1) rad x' y' angs
  where x' = x + 2 * rad * cos (ang + ang0)
        y' = y + 2 * rad * sin (ang + ang0)
