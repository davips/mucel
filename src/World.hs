module World (initialWorld, soma) where
import Config; import Geometry; import Bio; import Struct; import Rnd; import Debug
import Physics (timeToCollision)
import Data.List
import qualified Data.Vector as V

reduceVel :: Organism -> Organism
reduceVel org = updatePV org (orgPos org) (0.05 `sca` orgVel org)

w2 = width / 2

initialWorld = build timeToCollision $ combo $! initialWorld0

initialWorld1 = [ Uni $ ParticleInfo 0 (V (-400) 0) (V 100 0) 30
                , Uni $ ParticleInfo 1 (V 400 0) (V (-100) 8) 30
                , Uni $ ParticleInfo 2 (V (-50) 800) (V 0 (-2)) 100
                ]

combo l = if posis l == posis l' then l' else combo l'
  where l' = xm $ xp $ ym $ yp l -- $ correct l
        posis v = map orgPos v

xm [] = []
xm (x:xs) = r : xm xs
  where r = if orgPosX x - orgRad x < -width/2
            then updatePx x $ 1 - width/2 + orgRad x
            else x
xp [] = []
xp (x:xs) = r : xp xs
  where r = if orgPosX x + orgRad x > width/2
            then updatePx x $ -1 + width/2 - orgRad x
            else x

ym [] = []
ym (x:xs) = r : ym xs
  where r = if orgPosY x - orgRad x < -width/2
            then updatePy x $ 1 - width/2 + orgRad x
            else x

yp [] = []
yp (x:xs) = r : yp xs
  where r = if orgPosY x + orgRad x > width/2
            then updatePy x $ -1 + width/2 - orgRad x
            else x

correct :: [Organism] -> [Organism]
correct l = if posis vec == posis vec' then V.toList vec' else correct (V.toList vec')
  where vec = V.fromList l
        vec' = correct' vec 0 1
        posis v = V.map orgPos v

correct' :: V.Vector Organism -> Int -> Int -> V.Vector Organism
correct' vec ai bi
  | ai == length vec = vec
  | bi == length vec = correct' vec (ai + 1) (ai + 2)
  | otherwise        = correct' (V.unsafeUpd vec [(bi, b')]) ai (bi + 1)
    where b' = uninvade (V.unsafeIndex vec ai) (V.unsafeIndex vec bi)

uninvade oa ob = ob'
  where (a, b) = (orgPos oa, orgPos ob)
        ba = b `sub` a
        d = s + (orgRad oa + orgRad ob - dist a b)
        v = d `sca` uni ba
        ob' = if d > s then updatePV ob (b `add` v) (orgVel ob) else ob
        s = 1



initialWorld0 = walls ++ cells ++ combo circles
  where walls = [Wall $ ParticleInfo 0 (V w2 w2) (V 0 0) 0
              , Wall $ ParticleInfo 1 (V (-w2) w2) (V 0 0) 0
              , Wall $ ParticleInfo 2 (V (-w2) (-w2)) (V 0 0) 0
              , Wall $ ParticleInfo 3 (V w2 (-w2)) (V 0 0) 0]
        circles = concat [circle seed 10 (totalCells + 4) 14 0 0
                          , circle (seed+1) 12 (totalCells + 14) 14 500 500
                          , circle (seed+2) 60 (totalCells + 26) 21 (-500) (-700)
                          ]
        cells = [unic i x y vx vy r | (i, x, y, vx, vy, r) <- rtup]
        -- multicells = concat [multi i x y vx vy a n minRad (seed + 324 * i) | (i, x, y, vx, vy, a, n) <- rtupm]
        rtup = zip6 [4..totalCells + 3] (rpos $ seed + 243) (rpos $ seed + 745) (rvel $ seed + 452) (rvel $ seed + 245) (rrad $ seed + 356)
        -- rtupm = zip7 ids (rpos $ seed + 43) (rpos $ seed + 75) (rvel $ seed + 42) (rvel $ seed + 24) (rang $ seed + 56) ns
        -- rtupc :: [(Int, Int, Float, Float, Float)]
        -- rtupc = zip5 ns [last ids + last ns - 1 .. last ids + last ns + 2] (rpos $ seed + 413) (rpos $ seed + 715) (rrad $ seed + 35)
        -- ns = take totalMultis (rn $ seed + 36)
        -- ids = soma $ reverse $ (totalCells + 4) : (init ns)
        -- -- ids = d1 $ take totalMultis $ map (\(x, y) -> x+y) $ zip ((totalCells + 4) : init ns) ( 0 : (totalCells + 4) : ns)
        rpos = randomlist minPos maxPos
        rvel = randomlist minVel maxVel
        rrad = randomlist minRad maxRad
        rang = randomlist minChainAng maxChainAng
        -- rn seed = map round $ randomlist minSub maxSub seed

soma :: [Int] -> [Int]
soma [x] = [x]
soma (x:xs) = soma xs ++ [x + sum xs]

unic :: Int -> Float -> Float -> Float -> Float -> Float -> Organism
unic ido xpos ypos xvel yvel rad = Uni $ ParticleInfo ido (V xpos ypos) (V xvel yvel) rad

uni2subuni :: Vec -> Organism -> Organism
uni2subuni mc org = Uni $ SubParticleInfo ido p (dist p mc) (atan2 dy dx) r
  where V dx dy = p `sub` mc
        ParticleInfo ido p _ r = particleInfo org

multi :: Int -> Float -> Float -> Float -> Float -> Float -> Int -> Float -> Int -> [Organism]
-- multi ido xhead yhead xvel yvel avel n cellRad seed = Multi pinfo ainfo orgs
multi ido xhead yhead xvel yvel avel n cellRad seed = orgs
  where pinfo = ParticleInfo ido mc (V xvel yvel) rad
        ainfo = AngularInfo 0 avel
        -- orgs = map (uni2subuni mc) $ chain 0 (ido + 1) cellRad xhead yhead angs
        orgs = chain angs 0 vels ido cellRad xhead yhead
        angs = take n $ randomlist minChainAng maxChainAng seed
        vels = zip vxs vys
        vxs = take n $ randomlist minVelSub maxVelSub seed
        vys = take n $ randomlist minVelSub maxVelSub $ seed + 2435
        mc = massCenter posis
        rad = maxDist posis / 2 + cellRad
        posis = map orgPos orgs

circle :: Int -> Int -> Int -> Float -> Float -> Float -> [Organism]
circle seed n = chain (replicate n $ 2 * pi / fromIntegral n) 0 vels
  where vels = zip vxs vys
        vxs = take n $ randomlist minVelSub maxVelSub $ seed +3451
        vys = take n $ randomlist minVelSub maxVelSub $ seed + 245

chain :: [Float] -> Float -> [(Float,Float)]-> Int -> Float -> Float -> Float  -> [Organism]
chain _ _ [] _ _ _ _ = []
chain (ang:angs) ang0 ((vx,vy):vels) ido rad x y = unic ido x y vx (-200) rad : chain angs (ang + ang0) vels (ido + 1) rad x' y'
  where x' = x + 1.5 * rad * cos (ang + ang0)
        y' = y + 1.5 * rad * sin (ang + ang0)
-- chain :: Float -> Int -> Float -> Float -> Float -> [Float] -> [Organism]
-- chain _ _ _ _ _ []  = []
-- chain ang0 ido rad x y ((vx,vy):angs) = unic ido x y vx vy rad : chain (ang + ang0) (ido + 1) rad x' y' angs
--   where x' = x + 2 * rad * cos (ang + ang0)
--         y' = y + 2 * rad * sin (ang + ang0)
