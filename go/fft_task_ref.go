//====================================================
// Go FFT Task Reference
//====================================================

package main

import (
	"fmt"
	"math"
	"math/cmplx"
	"math/rand"
	"time"
)

// generateSignal creates a sine wave with added noise
func generateSignal(n int, freq float64, noise []float64) []float64 {
	signal := make([]float64, n)
	dt := 1.0 / float64(n)
	
	for i := 0; i < n; i++ {
		t := float64(i) * dt
		sineWave := math.Sin(2 * math.Pi * freq * t)
		signal[i] = sineWave + noise[i]
	}
	return signal
}

// generateSyntheticData generates synthetic data with random noise
func generateSyntheticData(n int, freq, noiseAmp float64) []float64 {
	rand.Seed(time.Now().UnixNano())
	noise := make([]float64, n)
	for i := range noise {
		noise[i] = noiseAmp * (2*rand.Float64() - 1) // Random values between -noiseAmp and noiseAmp
	}
	return generateSignal(n, freq, noise)
}

// movingAverageFilter applies a moving average filter to the signal
func movingAverageFilter(windowSize int, signal []float64) []float64 {
	if windowSize <= 0 {
		return signal
	}
	
	n := len(signal)
	filtered := make([]float64, n-windowSize+1)
	
	// Calculate first window sum
	windowSum := 0.0
	for i := 0; i < windowSize; i++ {
		windowSum += signal[i]
	}
	filtered[0] = windowSum / float64(windowSize)
	
	// Sliding window for remaining points
	for i := 1; i < len(filtered); i++ {
		windowSum = windowSum - signal[i-1] + signal[i+windowSize-1]
		filtered[i] = windowSum / float64(windowSize)
	}
	
	return filtered
}

// basicDFT computes the Discrete Fourier Transform
func basicDFT(signal []float64) []complex128 {
	n := len(signal)
	spectrum := make([]complex128, n)
	
	for k := 0; k < n; k++ {
		sum := complex(0, 0)
		for j := 0; j < n; j++ {
			angle := -2 * math.Pi * float64(k*j) / float64(n)
			sum += complex(signal[j], 0) * cmplx.Rect(1, angle)
		}
		spectrum[k] = sum
	}
	return spectrum
}

// findPeaks identifies peaks above the threshold in the spectrum
func findPeaks(threshold float64, spectrum []complex128) []int {
	peaks := []int{}
	
	for i := 1; i < len(spectrum)-1; i++ {
		mag := cmplx.Abs(spectrum[i])
		if mag > threshold &&
			mag > cmplx.Abs(spectrum[i-1]) &&
			mag > cmplx.Abs(spectrum[i+1]) {
			peaks = append(peaks, i)
		}
	}
	return peaks
}

func main() {
	// Generate 10,000 samples with frequency 10Hz and noise amplitude 0.1
	samples := generateSyntheticData(10000, 10.0, 0.1)
	
	// Print first 10 samples
	fmt.Println("First 10 samples:")
	for i := 0; i < 10; i++ {
		fmt.Printf("%.6f ", samples[i])
	}
	fmt.Println()
	
	// Apply moving average filter
	filtered := movingAverageFilter(50, samples)
	
	// Compute FFT
	spectrum := basicDFT(filtered)
	
	// Find peaks above threshold (adjust threshold as needed)
	peaks := findPeaks(1000.0, spectrum)
	fmt.Printf("\nFound %d peaks in the spectrum\n", len(peaks))
}
