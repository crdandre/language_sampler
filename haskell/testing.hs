import System.Random


-- this is the main entry point for the program
main :: IO ()
main = do
    print( doubleIt 5 )
    print( doubleIt 10 )
    print( fitLinear (generateRandom2DPoints 100) )

-- this defines i/o types for the function, then the function itself
doubleIt :: Int -> Int
doubleIt x = x * 2


-- | Generates a list of n random 2D points.
-- Input: n - number of points to generate
-- Output: List of (x,y) coordinates, where:
--   - Each coordinate is a Float between 0 and 100
--   - Points are deterministic (same seed always gives same points)
-- Example: generateRandom2DPoints 3 might return [(45.3,82.1), (12.7,67.4), (93.2,23.8)]
generateRandom2DPoints :: Int -> [(Float, Float)]
generateRandom2DPoints n = take n $ zip xs ys
  where
    gen1 = mkStdGen 42  -- Random generator for x coordinates
    gen2 = mkStdGen 43  -- Different seed for y coordinates
    xs = randomRs (0, 100) gen1  -- Generate x values between 0 and 100
    ys = randomRs (0, 100) gen2  -- Generate y values between 0 and 100


-- | Fits a 2-term linear function (y = mx + b) to a set of 2D points using least squares method.
-- Input: 
--   - points: A list of (x,y) coordinates
-- Output: A tuple (m, b) representing the slope and y-intercept of the line
-- Example:
-- let points = generateRandom2DPoints 100
-- let (slope, intercept) = fitLinear points
-- putStrLn $ "Slope: " ++ show slope ++ ", Y-intercept: " ++ show intercept
fitLinear :: [(Float, Float)] -> (Float, Float)
fitLinear points = (m, b)
  where
    n = fromIntegral $ length points
    sumX = sum $ map fst points
    sumY = sum $ map snd points
    sumXY = sum $ map (\(x, y) -> x * y) points
    sumX2 = sum $ map (\(x, _) -> x * x) points
    
    m = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    b = (sumY - m * sumX) / n

