module Main(main) where
import Bio
import Geometry
import Debug.Trace
import Data.Maybe
import Data.List 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate,ViewPort)

window :: (ViewPort -> Float -> World -> World) -> [Cell] -> IO ()
window step cells = simulate g black 500 (World cells) draw step
  where g = InWindow "bla" (round w, round h) (0, 0) --FullScreen (1920, 1080) 

draw :: World -> Picture
draw (World cells) = pictures $ (color white $ Line [(-w,-h), (-w,h), (w,h), (w,-h), (-w,-h)]) : map drawCell cells
 
update :: ViewPort -> Float -> World -> World
-- update _ dt (World bolas) = World $ map (move . electrify . luminify) bolas
--update _ dt (World cells) = World $ moveAll cells
update _ dt (World cells) = World $ traceShow (sum $ map ((^2).mag.vel) cells) $ moveAll cells

main :: IO ()
main = window update world