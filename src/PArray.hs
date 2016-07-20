module PArray (Horg, build, decreaseAll, updateAll, findMin, updateNode, Node(Node)) where
import Data.List
import Data.Function (on)
import qualified Data.PQueue.Prio.Min as H
type Horg = [Node]
data Node = Node {fstOrgId::Int, sndOrgId::Int, timeHeapNode::Float} deriving (Show, Eq)
instance Ord Node where compare = compare `on` timeHeapNode

build :: [Node] -> Horg
build x = x

-- Fast, but only for monotonic decreasing functions.
decreaseAll :: Float -> Horg -> Horg
decreaseAll dt = map (\x -> x {timeHeapNode = timeHeapNode x - dt})

updateAll :: (Node -> Node) -> Horg -> Horg
updateAll = map

findMin :: Horg -> Node
findMin = minimumBy (compare `on` timeHeapNode)

updateNode :: Node -> Float -> Node
updateNode pair newTime = pair {timeHeapNode = newTime}
