module Geometry(V(V),dist,stopPoint,isNear,decompoe,degrees,add,r,r2) where
import Data.List
import Data.Ord
data V = V{x::Float, y::Float} deriving Show
r = 30 :: Float
r2 = 2*r
smallest = 0.000001
degrees (V x y) = (atan2 y x) * 180 / 3.1415
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

isNear (V xi yi) (V dx dy) (V p q) = d < d / 2 + r2 && dpl < r2 --checar se p,q está nas costas
     where (x, y) = (xi+dx/2, yi+dy/2)
           (xf, yf) = (xi+dx, yi+dy)
           d = dist (V x y) (V p q)
           dpl = distpl (V p q) (V xi yi) (V xf yf)
           
decompoe :: V -> V -> V -> (V, V)
decompoe pa pb va = (vai, fica)
     where vai = sca t $ uni pd
           fica = sub va vai 
           pd = sub pb pa
           t = dot pd va

sca t (V x y) = V (t*x) (t*y)
sub (V ax ay) (V bx by) = V (ax-bx) (ay-by) 
dot (V ax ay) (V bx by) = ax*bx + ay*by
uni v@(V x y) = V (x/t) (y/t) where t = mag v
mag (V ax ay) = sqrt $ ax*ax + ay*ay