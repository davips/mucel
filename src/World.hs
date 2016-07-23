module World (initialWorld) where
import Config; import Geometry; import Bio; import Struct; import Rnd;
import Physics (timeToCollision)
import Data.List --; import Data.Maybe

initialWorld = wbuild orgId timeToCollision (walls ++ cells)
  where walls = [Wall $ ParticleInfo 0 (V (1000) (1000)) (V 0 0) 0
              , Wall $ ParticleInfo 1 (V (-1000) (1000)) (V 0 0) 0
              , Wall $ ParticleInfo 2 (V (-1000) (-1000)) (V 0 0) 0
              , Wall $ ParticleInfo 3 (V (1000) (-1000)) (V 0 0) 0]
        cells = [uni i x y vx vy r | (i, x, y, vx, vy, r) <- rtup]
        uni ido xpos ypos xvel yvel rad = Uni $ ParticleInfo ido (V xpos ypos) (V xvel yvel) rad
        rtup = zip6 ids (rpos 243) (rpos 745) (rvel 452) (rvel 245) (rrad 356)
        ids = [4..totalCells + 3]
        rpos = randomlist minPos maxPos
        rvel = randomlist minVel maxVel
        rrad = randomlist minRad maxRad
