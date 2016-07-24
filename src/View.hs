module View (window, update) where
import Debug; import Bio; import Physics; import Struct; import Geometry; import Config
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate, ViewPort)

w2 = width / 2

window stepFunction world = simulate g black fps world draw stepFunction
  where g = InWindow "bla" (round 800, round 600) (0, 0) --FullScreen (1920, 1080)

update _ timeStep world = evolve world timeStep

debug world = d2 (sum $ kinEnergy world)

draw world = color white $ Scale 0.15 0.15 $ pictures $ Line [(-w2,-w2), (-w2,w2), (w2,w2), (w2,-w2), (-w2,-w2)] : sprites
  where sprites = concatMap desenha $ wtoList world
        desenha u@(Uni _) = [drawSprite (orgPos u) $ orgRad u]
        -- desenha (Multi p v (org:orgs)) = [drawSprite (particlePos p) (particleRad p)]
        desenha (Multi p v (org:orgs)) = desenha org ++ desenha (Multi p v orgs)
        desenha _ = []

drawSprite :: Vec -> Float -> Picture
drawSprite (V x y) r = translate x y $ color blue $ circleSolid r
