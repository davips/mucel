module PQueue (Horg, build, decreaseAll, updateAll, findMin, updateNode, Node(Node)) where
import Data.Function (on)
import qualified Data.PQueue.Prio.Min as H
type Horg = H.MinPQueue Node Int
data Node = Node {fstOrgId::Int, sndOrgId::Int, timeHeapNode::Float} deriving (Show, Eq)
instance Ord Node where compare = compare `on` timeHeapNode

build :: [Node] -> Horg
build = H.fromList . (map tuplify) where tuplify x = (x, 0)

-- Fast, but only for monotonic decreasing functions.
decreaseAll :: Float -> Horg -> Horg
decreaseAll dt = H.mapKeysMonotonic (\x -> x {timeHeapNode = timeHeapNode x - dt})

updateAll :: (Node -> Node) -> Horg -> Horg
updateAll = H.mapKeys

findMin :: Horg -> Node
findMin = fst . H.findMin

updateNode :: Node -> Float -> Node
updateNode pair newTime = pair {timeHeapNode = newTime}
