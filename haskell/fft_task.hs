{-
This program implements a signal processing pipeline:
1. Generate 10,000 samples of synthetic data: y(t) = sin(2πft) + noise
2. Apply a moving average filter (window size = 50)
3. Compute FFT
4. Find peaks above threshold
5. Output timing metrics and results in README.md
-}
import Data.Complex
import System.Random -- required: cabal install --lib random

-- Pure function that generates signal given noise
generateSignal :: Int -> Double -> [Double] -> [Double]
generateSignal n freq noise = zipWith (+) sineWave (take n noise)
  where
    dt :: Double
    dt = 1.0 / fromIntegral n
    
    timePoints :: [Double]
    timePoints = [0.0, dt .. (fromIntegral n - 1) * dt]
    
    sineWave :: [Double]
    sineWave = [sin (2 * pi * freq * t) | t <- timePoints]

-- IO wrapper that handles random number generation
generateSyntheticData :: Int -> Double -> Double -> IO [Double]
generateSyntheticData n freq noiseAmp = do
    gen <- newStdGen
    let noise = randomRs (-noiseAmp, noiseAmp) gen
    return $ generateSignal n freq noise


movingAverageFilter :: Int -> [Double] -> [Double]
movingAverageFilter windowSize signal
    -- Handle invalid window size
    | windowSize <= 0 = signal
    | otherwise = map calculateWindowAverage slidingWindows --if not returning the original data, map A to B
    where
        -- Calculate average of a single window
        calculateWindowAverage :: [Double] -> Double
        calculateWindowAverage window = 
            sum window / fromIntegral windowSize

        -- Create all sliding windows from the signal
        slidingWindows :: [[Double]]
        slidingWindows = 
            [ take windowSize (drop i signal) 
            | i <- [0..length signal - windowSize] ]

--  basic DFT
basicDFT :: [Double] -> [Complex Double]
basicDFT signal = 
    [ sum [ (x :+ 0) * omega k n | (x, n) <- zip signal [0..] ]
    | k <- [0..length signal - 1] ]
    where
        n = length signal
        omega k n = cis (-2 * pi * fromIntegral (k * n) / fromIntegral (length signal))


-- could do cooley tukey FFT here as well...

-- find peaks above threshold
findPeaks :: Double -> [Complex Double] -> [Int]
findPeaks threshold spectrum = 
    [ i 
    | (i, val) <- zip [0..] spectrum
    , magnitude val > threshold  -- magnitude from Data.Complex
    , isPeak i val
    ]
    where
        isPeak i val = 
            i > 0 && i < length spectrum - 1 &&  -- not first or last point
            magnitude val > magnitude (spectrum !! (i-1)) &&  -- greater than previous
            magnitude val > magnitude (spectrum !! (i+1))     -- greater than next


getTimingMetrics :: [Double] -> IO ()
getTimingMetrics signal = do
    let filteredSignal = movingAverageFilter 50 signal
    let fftSignal = basicDFT filteredSignal
    let peaks = findPeaks 0.1 fftSignal
    print peaks


main :: IO ()
main = do
    samples <- generateSyntheticData 10000 10.0 0.1
    print $ take 10 samples


