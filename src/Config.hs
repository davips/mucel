module Config (minVelSub, maxVelSub, fps, activateDebug, width, seed, totalMultis, totalCells, minRad, maxRad, minPos, maxPos, minVel, maxVel, verySmallFloat, veryLargeFloat, minChainAng, maxChainAng, minSub, maxSub, kinMonitor, invMonitor, gravity) where
activateDebug = True

totalCells = 6 :: Int
totalMultis = 0 :: Int
fps = 20 :: Int
width = 2000 :: Float
seed = 25860 :: Int
(minRad, maxRad) = (15, 60) :: (Float, Float)
(minPos, maxPos) = (-950, 950) :: (Float, Float)
(minVel, maxVel) = (-700, 700) :: (Float, Float)
(minVelSub, maxVelSub) = (-1, 1)::(Float, Float)

(minSub, maxSub) = (2, 15) :: (Float, Float)
(minChainAng, maxChainAng) = (-pi / 3, pi / 3) :: (Float, Float)

gravity =9.8:: Float
(verySmallFloat, veryLargeFloat) = (0.0000000001, 9999999999) :: (Float, Float)
kinMonitor = False
invMonitor = False
