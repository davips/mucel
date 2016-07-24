module Config (fps, activateDebug, width, seed, totalMultis, totalCells, minRad, maxRad, minPos, maxPos, minVel, maxVel, verySmallFloat, veryLargeFloat, minChainAng, maxChainAng, minSub, maxSub) where
activateDebug = True

totalCells = 30 :: Int
totalMultis = 10 :: Int
fps = 30 :: Int
width = 2000 :: Float
seed = 25256 :: Int
(minRad, maxRad) = (15, 40) :: (Float, Float)
(minPos, maxPos) = (-800, 800) :: (Float, Float)
(minVel, maxVel) = (-500, 500) :: (Float, Float)

(minSub, maxSub) = (2, 20) :: (Float, Float)
(minChainAng, maxChainAng) = (-pi / 3, pi / 3) :: (Float, Float)

(verySmallFloat, veryLargeFloat) = (0.000000001, 3600 * 24 * 365 * 1000) :: (Float, Float)
