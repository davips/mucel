module World (initialWorld) where
import Config; import Geometry; import Bio; import Struct; import Rnd;
import Physics (timeToCollision)
import Data.List (zip6)

w2 = width / 2

initialWorld = wbuild timeToCollision (walls ++ cells)
  where walls = [Wall $ ParticleInfo 0 (V w2 w2) (V 0 0) 0
              , Wall $ ParticleInfo 1 (V (-w2) w2) (V 0 0) 0
              , Wall $ ParticleInfo 2 (V (-w2) (-w2)) (V 0 0) 0
              , Wall $ ParticleInfo 3 (V w2 (-w2)) (V 0 0) 0]
        cells = [uni i x y vx vy r | (i, x, y, vx, vy, r) <- rtup]
        uni ido xpos ypos xvel yvel rad = Uni $ ParticleInfo ido (V xpos ypos) (V xvel yvel) rad
        rtup = zip6 ids (rpos $ seed + 243) (rpos $ seed + 745) (rvel $ seed + 452) (rvel $ seed + 245) (rrad $ seed + 356)
        ids = [4..totalCells + 3]
        rpos = randomlist minPos maxPos
        rvel = randomlist minVel maxVel
        rrad = randomlist minRad maxRad
