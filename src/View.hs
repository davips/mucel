module View (window, update) where
import Bio; import Geometry; import Graphics.Gloss; import Debug.Trace
import Graphics.Gloss.Interface.Pure.Simulate (simulate,ViewPort)
import qualified Data.Vector as Ve

(w, h, fps) = (800, 600, 30)

window :: (ViewPort -> Float -> World -> World) -> World -> IO ()
window stepFunction world = simulate g black fps world draw stepFunction
  where g = InWindow "bla" (round w, round h) (0, 0) --FullScreen (1920, 1080)

update :: ViewPort -> Float -> World -> World
update _ timeStep world = evolve world timeStep

debug :: World -> b -> b
debug world x = traceShow (sum $ kinEnergy $ Ve.toList (organisms world)) x
  where kinEnergy = map ((^2) . mag . orgVel)

orgVel = particleVel . particleInfo
orgPos = particlePos . particleInfo
orgRad = particleRad . particleInfo

draw :: World -> Picture
draw (World orgs _) = color white $ Scale 0.15 0.15 $ pictures $ Line [(-1000,-1000), (-1000,1000), (1000,1000), (1000,-1000), (-1000,-1000)] : sprites
  where sprites = Ve.toList $ Ve.map (\x -> drawSprite (orgPos x) $ orgRad x) orgs

drawSprite :: Vec -> Float -> Picture
drawSprite (V x y) r = translate x y $ color blue $ circleSolid r
