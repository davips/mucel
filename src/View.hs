module View (window, update) where
import Debug; import Bio; import Physics; import Struct; import Geometry
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate,ViewPort)

(w, h, fps) = (800, 600, 30)

window stepFunction world = simulate g black fps world draw stepFunction
  where g = InWindow "bla" (round w, round h) (0, 0) --FullScreen (1920, 1080)

update _ timeStep world = evolve world timeStep

debug world = debug2 (sum $ kinEnergy world)

draw world = color white $ Scale 0.15 0.15 $ pictures $ Line [(-1000,-1000), (-1000,1000), (1000,1000), (1000,-1000), (-1000,-1000)] : sprites
  where sprites = wtoList $ wmap (\x -> drawSprite (orgPos x) $ orgRad x) world

drawSprite :: Vec -> Float -> Picture
drawSprite (V x y) r = translate x y $ color blue $ circleSolid r
