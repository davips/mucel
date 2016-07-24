module View (window, update) where
import Debug; import Bio; import Physics; import Struct; import Geometry; import Config
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate, ViewPort)

w2 = width / 2

window stepFunction world = simulate g black fps world draw stepFunction
  where g = InWindow "bla" (round 800, round 600) (0, 0) --FullScreen (1920, 1080)

update _ timeStep world = evolve world timeStep

debug world = debug2 (sum $ kinEnergy world)

draw world = color white $ Scale 0.15 0.15 $ pictures $ Line [(-w2,-w2), (-w2,w2), (w2,w2), (w2,-w2), (-w2,-w2)] : sprites
  where sprites = wtoList $ wmapi (\x -> drawSprite (orgPos x) $ orgRad x) world

drawSprite :: Vec -> Float -> Picture
drawSprite (V x y) r = translate x y $ color blue $ circleSolid r
