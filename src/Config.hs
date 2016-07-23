module Config (activateDebug, totalCells, minRad, maxRad, minPos, maxPos, minVel, maxVel, verySmallFloat, veryLargeFloat) where
activateDebug = False

totalCells = 100 :: Int
(minRad, maxRad) = (5, 40) :: (Float, Float)
(minPos, maxPos) = (-850, 850) :: (Float, Float)
(minVel, maxVel) = (-500, 500) :: (Float, Float)

(verySmallFloat, veryLargeFloat) = (0.000000001, 3600 * 24 * 365 * 1000) :: (Float, Float)
