module Main(main) where
import Bio
import Debug.Trace
import Data.Maybe
import Data.List 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate,ViewPort)

window :: (ViewPort -> Float -> World -> World) -> [Cell] -> IO ()
window step cells = simulate g black 30 (World cells) draw step
     where g = InWindow "bla" (1800, 900) (0, 0)

draw :: World -> Picture
draw (World cells) = pictures $ map drawCell cells
 
update :: ViewPort -> Float -> World -> World
-- update _ dt (World bolas) = World $ map (move . electrify . luminify) bolas
update _ dt (World cells) = {-traceShow cells $ -} World $ moveAll cells

main :: IO ()
main = window update world