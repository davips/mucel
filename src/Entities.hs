module Entities (Organism) where
type Horg = H.MinPQueue HeapNode Float
type Orgs = Ve.Vector Organism
data World = World {organisms::Orgs, collisionHeap::Horg}
data HeapNode = Node {fstOrgId::Int, sndOrgId::Int, timeHeapNode::Float} deriving (Show, Eq)
instance Ord HeapNode where compare = compare `on` timeHeapNode
data Organism = Uni {particleInfo::ParticleInfo}
              | Multi {particleInfo::ParticleInfo, aInfo::AngularInfo, subOrgs::[Organism]}
              | Wall {particleInfo::ParticleInfo} deriving (Show)
