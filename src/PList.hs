module PList (TimeList, build, decreaseAll, updateAll, findMin, updateNode, Node(Node), getNode, nodeTime) where
import Data.List
import Data.Function (on)

type TimeList = [Node]
data Node = Node {first :: Int, second :: Int, nodeTime :: Float} deriving (Show, Eq)
instance Ord Node where compare = compare `on` nodeTime

build :: [Node] -> TimeList
build x = x

-- Fast, but only for monotonic decreasing functions.
decreaseAll :: Float -> TimeList -> TimeList
decreaseAll dt = map (\x -> x {nodeTime = nodeTime x - dt})

updateAll :: (Node -> Node) -> TimeList -> TimeList
updateAll = map

findMin :: TimeList -> [Node]
findMin l = filter (\(Node a b t) -> t == min) l
  where min = minimum $ map nodeTime l

findMins :: TimeList -> [Node]
findMins l = sort l

updateNode :: Node -> Float -> Node
updateNode pair newTime = pair {nodeTime = newTime}

getNode :: Int -> Int -> [Node] -> Node
getNode a b l = head $ filter f l
  where f (Node x y _) = a == x && b == y
