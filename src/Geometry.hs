module Geometry(V(V),mag,sca,dist,stopPoint,isNear,decompoe,degrees,add,r,r2,timeToHit,Ball(Ball),ballId,buildPairsMap) where

import Data.Maybe
import qualified Data.Map.Strict as Map
import Debug.Trace
import Data.List
import Data.Ord
import Data.Function (on)
data V = V{x::Float, y::Float} deriving Show
data Ball = Ball{ballId::Int, ballP::V, ballV::V, ballR::Float}
instance Eq Ball where Ball ida _ _ _ == Ball idb _ _ _ = ida == idb
instance Ord Ball where compare = compare `on` ballId

r = 10 :: Float
r2 = 2*r
smallest = 0.0005
degrees (V x y) = (atan2 y x) * 180 / pi
--collides cella cellb = dist (x cella) (y cella) (x cellb) (y cellb) < r2
add (V x1 y1) (V x2 y2) = V (x1+x2) (y1+y2)
dist (V x1 y1) (V x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
distpl (V x0 y0) (V x1 y1) (V x2 y2) = abs(y21*x0 - x21*y0 + x2*y1 - y2*x1) / (sqrt (y21*y21 + x21*x21))
     where y21=y2 -y1
           x21=x2 -x1
stopPoint (V x y) (V dx dy) (V p q) = if dist (V x y) (V sx sy) < dist (V x y) (V sx' sy') then (V sx sy) else (V sx' sy')
     where
          m = dy / dx
          c = y - m*x
          (dA2,dB,dC,dRoot) = (2*(m*m + 1), 2*(m*c - m*q - p), q*q - r2*r2 + p*p - 2*c*q + c*c, sqrt (dB*dB - 2*dA2*dC))
          (sx, sy) = if abs dx < smallest then (x, (-eB + eRoot) / eA2) else ((-dB + dRoot) / dA2, m*sx + c) 
          (sx', sy') = if abs dx < smallest then (x, (-eB - eRoot) / eA2) else ((-dB - dRoot) / dA2, m*sx' + c)
          (eA2,eB,eC,eRoot) = (2, -2*q, q*q + (x-p)*(x-p) - r2*r2, sqrt(eB*eB - 2*eA2*eC))          

isNear ai@(V xi yi) v@(V dx dy) b@(V p q) = canaletou && atingivel
     where
          af = add ai v
          canaletou = distpl b ai af < r2 
          ab = sub b ai
          atingivel = proj > 0 && (dist ai b - proj < r2)
          proj = dot (uni ab) v
           
decompoe :: V -> V -> V -> (V, V)
decompoe pa pb va = (fica, vai)
     where vai = sca t $ uni pd
           fica = sub va vai 
           pd = sub pb pa
           t = dot (uni pd) va

sca t (V x y) = V (t*x) (t*y)
sub (V ax ay) (V bx by) = V (ax-bx) (ay-by) 
dot (V ax ay) (V bx by) = ax*bx + ay*by
uni v@(V x y) = V (x/t) (y/t) where t = mag v
mag = sqrt . magSquared
magSquared (V ax ay) = ax*ax + ay*ay


buildPairsMap :: [Ball] -> Map.Map (Ball, Ball) Float
buildPairsMap balls = Map.fromList $ buildPairsMap' balls
buildPairsMap' :: [Ball] -> [((Ball, Ball), Float)]
buildPairsMap' (ball:balls) = oneToN ++ buildPairsMap' balls
  where
    oneToN = map (\ball2 -> ((ball, ball2), fromJust $ timeToHit ball ball2)) balls 

timeToHit (Ball _ p1 s1 r1) (Ball _ p2 s2 r2) = if baskaD <= 0 then Nothing else Just t
  where
    baska2A = 2 * (mag s1s2) ^ 2
    baskamB = -2 * (dot p1p2 s1s2)
    baskaC = (magSquared p1p2) - (r1 + r2) ^ 2
    baskaD = baskamB ^ 2 - 2 * baska2A * baskaC
    baskaDRooted = sqrt(baskaD)
    tp = (baskamB + baskaDRooted) / baska2A
    tm = (baskamB - baskaDRooted) / baska2A
    t0 = min tp tm
    t = if (t0 < 0) then error "Ball integrity violated!" else t0
    p1p2 = sub p1 p2
    s1s2 = sub s1 s2