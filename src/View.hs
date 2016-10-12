module View (window, update) where
import Debug; import Bio; import Physics; import Struct; import Geometry; import Config
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate (simulate, ViewPort)
import qualified Data.Vector as V

w2 = width / 2

window stepFunction world = simulate g black fps world draw stepFunction
  where g = InWindow "mucel" (round 700, round 700) (0, 0) --FullScreen (1920, 1080)

update _ timeStep world = evolve world timeStep

d2k :: [Organism] -> [Organism]
d2k world = if kinMonitor then d2 (sum $ kinEnergy world) world else world

verify :: World -> Picture -> Picture
verify world pic = if invMonitor then if null invasions then pic else d2 invasions pic else pic
  where invasions = filter (\(_, _, x) -> x < -verySmallFloat) dists
        dists = [(a, b, distOrgs a b - orgRad a - orgRad b) | a <- os, b <- os, idn a < idn b]
        os = V.toList $ sitems world

org2part :: Organism -> [ParticleInfo]
org2part (Uni p _) = [p]
org2part (Multi _ cells) = concatMap org2part cells
org2part (Wall _) = []

draw :: World -> Picture
draw world = verify world $ color white $ Scale 0.34 0.34 $ pictures $ quadro : sprites
  where quadro = Line [(-w2,-w2), (-w2,w2), (w2,w2), (w2,-w2), (-w2,-w2)]
        sprites = map desenha $ concatMap org2part $ d2k $ V.toList $ sitems world
        desenha part = drawSprite (particlePos part) (particleRad part)

drawSprite :: Vec -> Float -> Picture
drawSprite (V x y) r = translate x y $ color blue $ circleSolid r
