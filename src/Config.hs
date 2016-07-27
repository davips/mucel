module Config (minVelSub, maxVelSub, fps, activateDebug, width, seed, totalMultis, totalCells, minRad, maxRad, minPos, maxPos, minVel, maxVel, verySmallFloat, veryLargeFloat, minChainAng, maxChainAng, minSub, maxSub, kinMonitor, invMonitor, gravity) where
activateDebug = True

totalCells = 3 :: Int
totalMultis = 0 :: Int
fps = 30 :: Int
width = 2000 :: Float
seed = 25860 :: Int
(minRad, maxRad) = (30, 80) :: (Float, Float)
(minPos, maxPos) = (-950, 950) :: (Float, Float)
(minVel, maxVel) = (-200, 200) :: (Float, Float)
(minVelSub, maxVelSub) = (-1, 1) :: (Float, Float)

(minSub, maxSub) = (2, 15) :: (Float, Float)
(minChainAng, maxChainAng) = (-pi / 3, pi / 3) :: (Float, Float)

gravity = 100 :: Float
(verySmallFloat, veryLargeFloat) = (0.0000000001, 9999999999) :: (Float, Float)
kinMonitor = False
invMonitor = False
