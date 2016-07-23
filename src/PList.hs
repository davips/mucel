module PList (TimeList, build, decreaseAll, updateAll, findMin, updateNode, Node(Node)) where
import Data.List
import Data.Function (on)

type TimeList = [Node]
data Node = Node {first :: Int, second :: Int, time :: Float} deriving (Show, Eq)
instance Ord Node where compare = compare `on` time

build :: [Node] -> TimeList
build x = x

-- Fast, but only for monotonic decreasing functions.
decreaseAll :: Float -> TimeList -> TimeList
decreaseAll dt = map (\x -> x {time = time x - dt})

updateAll :: (Node -> Node) -> TimeList -> TimeList
updateAll = map

findMin :: TimeList -> Node
findMin = minimumBy (compare `on` time)

updateNode :: Node -> Float -> Node
updateNode pair newTime = pair {time = newTime}
