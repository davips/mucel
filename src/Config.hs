module Config (fps, activateDebug, width, seed, totalCells, minRad, maxRad, minPos, maxPos, minVel, maxVel, verySmallFloat, veryLargeFloat) where
activateDebug = True

totalCells = 30 :: Int
fps = 30 :: Int
width = 2000 :: Float
seed = 25256 :: Int
(minRad, maxRad) = (2, 40) :: (Float, Float)
(minPos, maxPos) = (-850, 850) :: (Float, Float)
(minVel, maxVel) = (-500, 500) :: (Float, Float)

(verySmallFloat, veryLargeFloat) = (0.000000001, 3600 * 24 * 365 * 1000) :: (Float, Float)
