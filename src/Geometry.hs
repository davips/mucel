module Geometry(Vec(V),dist, AngularInfo(AngularInfo), particleId, particlePos, particleVel, particleRad, mag,sca,add,timeToHit,ParticleInfo(ParticleInfo)) where

-- import qualified Data.Heap as H
-- import Data.Maybe
-- import qualified Data.Map.Strict as M
-- import Debug.Trace
-- import Data.List
-- import Data.Ord
import Data.Function (on)

data Vec = V{x::Float, y::Float} deriving Show
instance Eq ParticleInfo where ParticleInfo ida _ _ _ == ParticleInfo idb _ _ _ = ida == idb
instance Ord ParticleInfo where compare = compare `on` particleId

data ParticleInfo = ParticleInfo {particleId::Int, particlePos::Vec, particleVel::Vec, particleRad::Float} deriving (Show)
data AngularInfo = AngularInfo {angPos::Vec, angVel::Vec} deriving (Show)

sca t (V x y) = V (t*x) (t*y)
sub (V ax ay) (V bx by) = V (ax-bx) (ay-by)
dot (V ax ay) (V bx by) = ax*bx + ay*by
uni v@(V x y) = V (x/t) (y/t) where t = mag v
mag = sqrt . magSquared
magSquared (V ax ay) = ax*ax + ay*ay

timeToHit :: ParticleInfo -> ParticleInfo -> Maybe Float
timeToHit (ParticleInfo _ p1 s1 r1) (ParticleInfo _ p2 s2 r2) = if baskaD <= 0 then Nothing else Just t
  where
    baska2A = 2 * mag s1s2 ^ 2
    baskamB = -2 * p1p2 `dot` s1s2
    baskaC = magSquared p1p2 - (r1 + r2) ^ 2
    baskaD = baskamB ^ 2 - 2 * baska2A * baskaC
    baskaDRooted = sqrt baskaD
    tp = (baskamB + baskaDRooted) / baska2A
    tm = (baskamB - baskaDRooted) / baska2A
    t0 = min tp tm
    t = if t0 < 0 then error "ParticleInfo integrity violated!" else t0
    p1p2 = p1 `sub` p2
    s1s2 = s1 `sub` s2

add (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)
dist (V x1 y1) (V x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)






-- r = 10 :: Float
-- r2 = 2*r
-- smallest = 0.0005
-- degrees (V x y) = atan2 y x * 180 / pi
-- --collides cella cellb = dist (x cella) (y cella) (x cellb) (y cellb) < r2
-- add (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)
-- dist (V x1 y1) (V x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
-- distpl (V x0 y0) (V x1 y1) (V x2 y2) = abs(y21*x0 - x21*y0 + x2*y1 - y2*x1) / sqrt (y21*y21 + x21*x21)
--      where y21=y2 -y1
--            x21=x2 -x1
--
-- stopPoint :: Vec -> Vec -> Vec -> Vec
-- stopPoint (V x y) (V dx dy) (V p q) = if dist (V x y) (V sx sy) < dist (V x y) (V sx' sy') then V sx sy else V sx' sy'
--      where
--           m = dy / dx
--           c = y - m*x
--           (dA2,dB,dC,dRoot) = (2*(m*m + 1), 2*(m*c - m*q - p), q*q - r2*r2 + p*p - 2*c*q + c*c, sqrt (dB*dB - 2*dA2*dC))
--           (sx, sy) = if abs dx < smallest then (x, (-eB + eRoot) / eA2) else ((-dB + dRoot) / dA2, m*sx + c)
--           (sx', sy') = if abs dx < smallest then (x, (-eB - eRoot) / eA2) else ((-dB - dRoot) / dA2, m*sx' + c)
--           (eA2,eB,eC,eRoot) = (2, -2*q, q*q + (x-p)*(x-p) - r2*r2, sqrt(eB*eB - 2*eA2*eC))
--
-- isNear ai@(V xi yi) v@(V dx dy) b@(V p q) = canaletou && atingivel
--      where
--           af = add ai v
--           canaletou = distpl b ai af < r2
--           ab = sub b ai
--           atingivel = proj > 0 && (dist ai b - proj < r2)
--           proj = dot (uni ab) v
--
-- decompoe :: Vec -> Vec -> Vec -> (Vec, Vec)
-- decompoe pa pb va = (fica, vai)
--      where vai = sca t $ uni pd
--            fica = sub va vai
--            pd = sub pb pa
--            t = dot (uni pd) va



-- buildHeap :: [ParticleInfo] -> HeapT (Prio MinPolicy A) () -> M.Map (ParticleInfo, ParticleInfo) Float
-- buildHeap balls = M.fromList $ buildHeap' balls
-- buildHeap' :: [ParticleInfo] -> [((ParticleInfo, ParticleInfo), Float)]
-- buildHeap' (ball:balls) = oneToN ++ buildHeap' balls
--   where
--     oneToN = map (\ball2 -> ((ball, ball2), fromJust $ timeToHit ball ball2)) balls
