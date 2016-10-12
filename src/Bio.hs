module Bio(World, Organism(Wall, Uni, Multi), particleInfo, orgId, orgPos, orgVel, orgRad, updatePV, updatePx, updatePy, distOrgs, orgPosX, orgPosY, subOrgs) where
import Config; import Geometry
import Struct (Struct, Identifyable, idn)
import Dna

type World = Struct Organism
data Organism
    = Uni { particleInfo :: ParticleInfo}
    | Multi { particleInfo :: ParticleInfo
            , aInfo :: AngularInfo
            , subOrgs :: [Organism]}
    | Wall { particleInfo :: ParticleInfo}
    deriving ((Show))
instance Identifyable Organism where idn = orgId
instance Eq Organism where a == b = orgId a == orgId b

orgId :: Organism -> Int
orgId = particleId . particleInfo
orgPos :: Organism -> Vec
orgPos = particlePos . particleInfo
(orgPosX,orgPosY) = (vecX . particlePos . particleInfo, vecY . particlePos . particleInfo)
orgVel = particleVel . particleInfo
orgRad = particleRad . particleInfo
updatePV org p v = org { particleInfo = (particleInfo org) { particlePos = p, particleVel = v } }
updateP org p = org {particleInfo = (particleInfo org) {particlePos = p}}
updatePx org x = org { particleInfo = (particleInfo org) { particlePos = V x (vecY $ orgPos org) } }
updatePy org y = org {particleInfo = (particleInfo org) {particlePos = V (vecX  $ orgPos org) y }}
distOrgs a b = dist (orgPos a) (orgPos b)
