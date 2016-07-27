module Geometry(Vec(V), vecX, vecY, dist, AngularInfo(AngularInfo), roundResidual, veryLargeFloat, particleId, particlePos, particleVel, particleRad, mag,sca,add,timeToHit,ParticleInfo(ParticleInfo, SubParticleInfo), decompoe, distPointToLine, timeToHitWall, move, movea, mean, maxDist, massCenter, sub, angPos, uni) where
import Config
import Data.Function (on)
import Debug
import Data.List

data Vec = V{vecX::Float, vecY::Float} deriving (Eq, Show)
data ParticleInfo = ParticleInfo {particleId::Int, particlePos::Vec, particleVel::Vec, particleRad::Float}
                  | SubParticleInfo {particleId::Int, particlePos::Vec, particleDist::Float, particleAng::Float, particleRad::Float} deriving (Show)
instance Eq ParticleInfo where ParticleInfo ida _ _ _ == ParticleInfo idb _ _ _ = ida == idb
instance Ord ParticleInfo where compare = compare `on` particleId
data AngularInfo = AngularInfo {angPos::Float, angVel::Float} deriving (Show)

sca t (V x y) = V (t*x) (t*y)
sub (V ax ay) (V bx by) = V (ax-bx) (ay-by)
dot (V ax ay) (V bx by) = ax*bx + ay*by
uni v@(V x y) = V (x/t) (y/t) where t = mag v
mag = sqrt . magSquared
magSquared (V ax ay) = ax*ax + ay*ay
roundResidual x | abs x < verySmallFloat = 0 :: Float
                | otherwise = verySmallFloat

timeToHitWall (ParticleInfo _ (V px py) (V vx vy) r) (ParticleInfo i (V ax ay) _ _)
    | i == 0 = tempo py vy r ay
    | i == 2 = tempom py vy r ay
    | i == 1 = tempom px vx r ax
    | i == 3 = tempo px vx r ax
      where tempo p v r a = if v <= 0 then Nothing else Just $ (a - p - r) / v
            tempom p v r a = if v >= 0 then Nothing else Just $ (p - a - r) / (-v)

timeToHit :: ParticleInfo -> ParticleInfo -> Maybe Float
timeToHit (ParticleInfo _ p1 s1 r1') (ParticleInfo _ p2 s2 r2') = if baskaD < 0 then Nothing else maybeT
  where
    baska2A = 2 * mag s1s2 ^ 2
    baskamB = -2 * p1p2 `dot` s1s2
    baskaC = p1p2sq - r1r2 ^ 2
    baskaD = baskamB ^ 2 - 2 * baska2A * baskaC
    baskaDRooted = sqrt baskaD --error "Particle integrity violated!"
    tp = (baskamB + baskaDRooted) / baska2A
    tm = (baskamB - baskaDRooted) / baska2A
    (t0, r1, r2) = if sqrt p1p2sq < 0.99 * (r1' + r2') -- se há intersecção suficiente, aceita que a segunda solução seja negativa
                   then (max tp tm, 0.9899 * r1', 0.9899 * r2') -- reduz raio pra evitar escape, mas sem reduzir a ponto de ficar sem intersecção
                   else (min tp tm, r1', r2')
    p1p2sq = magSquared p1p2
    r1r2 = r1 + r2

    maybeT = if t0 <= 0 then Nothing else Just t0 -- t == 0 é ambíguo, então considera sem colisão; embora nunca deva ocorrer
    p1p2 = p1 `sub` p2
    s1s2 = s1 `sub` s2

move :: Float -> ParticleInfo -> ParticleInfo
move 0 x = x
move dt p@(ParticleInfo oid oldPos vel _) = p {particlePos = oldPos `add` (dt `sca` vel)}

movea :: Float -> AngularInfo -> AngularInfo
movea 0 x = x
movea dt a@(AngularInfo pos vel) = a {angPos = pos + dt * vel}

add (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)
dist (V x1 y1) (V x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

distPointToLine (V x0 y0) (V x1 y1) (V x2 y2) = abs(y21*x0 - x21*y0 + x2*y1 - y2*x1) / sqrt (y21*y21 + x21*x21)
  where y21=y2 -y1
        x21=x2 -x1

decompoe :: Vec -> Vec -> Vec -> (Vec, Vec)
decompoe pa pb va = (fica, vai)
     where vai = sca t $ uni pd
           fica = sub va vai
           pd = sub pb pa
           t = dot (uni pd) va

projectedVec :: Vec -> Vec -> Vec -> Vec
projectedVec v a b = (uba `dot` v) `sca` uba
  where uba = uni $ b `sub` a
-- degrees (V x y) = atan2 y x * 180 / pi

mean xs = realToFrac (sum xs) / genericLength xs

maxDist :: [Vec] -> Float
maxDist xs = maximum [dist a b | a <- xs, b <- xs]

massCenter :: [Vec] -> Vec
massCenter vecs = V (mean $ map vecX vecs) (mean $ map vecY vecs)
