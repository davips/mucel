module Main(main) where
import Bio
import Geometry
import Debug.Trace
import Data.Maybe
import Data.List 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate,ViewPort)

w = 1360
h = 750
window :: (ViewPort -> Float -> World -> World) -> [Cell] -> IO ()
window step cells = simulate g black 500 (World cells) draw step
  where g = InWindow "bla" (w, h) (0, 0) --FullScreen (1920, 1080) 

draw :: World -> Picture
draw (World cells) = pictures $ map drawCell cells
 
update :: ViewPort -> Float -> World -> World
-- update _ dt (World bolas) = World $ map (move . electrify . luminify) bolas
--update _ dt (World cells) = World $ moveAll cells
update _ dt (World cells) = World $ moveAll cells

main :: IO ()
main = window update world