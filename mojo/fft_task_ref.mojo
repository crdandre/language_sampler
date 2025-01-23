//====================================================
// Mojo FFT Task Reference
//====================================================

from python import Python
from math import sin, pi, sqrt
from complex import Complex
from random import random_float64
from time import now
from memory.unsafe import Pointer

fn generate_signal(n: Int, freq: Float64, noise_amp: Float64) -> DynamicVector[Float64]:
    var signal = DynamicVector[Float64](n)
    let dt: Float64 = 1.0 / Float64(n)
    
    for i in range(n):
        let t = Float64(i) * dt
        let sine_val = sin(2 * pi * freq * t)
        let noise = (random_float64() * 2 - 1) * noise_amp
        signal.push_back(sine_val + noise)
    
    return signal

fn moving_average_filter(signal: DynamicVector[Float64], window_size: Int) -> DynamicVector[Float64]:
    if window_size <= 0:
        return signal
        
    var filtered = DynamicVector[Float64](len(signal) - window_size + 1)
    
    for i in range(len(signal) - window_size + 1):
        var sum: Float64 = 0
        for j in range(window_size):
            sum += signal[i + j]
        filtered.push_back(sum / Float64(window_size))
    
    return filtered

fn basic_dft(signal: DynamicVector[Float64]) -> DynamicVector[Complex[Float64]]:
    let n = len(signal)
    var spectrum = DynamicVector[Complex[Float64]](n)
    
    for k in range(n):
        var sum = Complex[Float64](0, 0)
        for t in range(n):
            let angle = -2 * pi * Float64(k * t) / Float64(n)
            let omega = Complex[Float64](cos(angle), sin(angle))
            sum += omega * Complex[Float64](signal[t], 0)
        spectrum.push_back(sum)
    
    return spectrum

fn magnitude(c: Complex[Float64]) -> Float64:
    return sqrt(c.re * c.re + c.im * c.im)

fn find_peaks(spectrum: DynamicVector[Complex[Float64]], threshold: Float64) -> DynamicVector[Int]:
    var peaks = DynamicVector[Int]()
    let n = len(spectrum)
    
    for i in range(1, n-1):
        let mag = magnitude(spectrum[i])
        if mag > threshold:
            let prev_mag = magnitude(spectrum[i-1])
            let next_mag = magnitude(spectrum[i+1])
            if mag > prev_mag and mag > next_mag:
                peaks.push_back(i)
    
    return peaks

fn main() raises:
    # Parameters
    let n = 10000
    let freq = 10.0
    let noise_amp = 0.1
    let window_size = 50
    let threshold = 1000.0  # Adjust based on your needs
    
    # Generate synthetic data
    let start_time = now()
    let signal = generate_signal(n, freq, noise_amp)
    
    # Apply moving average filter
    let filtered_signal = moving_average_filter(signal, window_size)
    
    # Compute FFT (using basic DFT implementation)
    let spectrum = basic_dft(filtered_signal)
    
    # Find peaks
    let peaks = find_peaks(spectrum, threshold)
    
    # Print results
    print("First 10 samples:", end="")
    for i in range(10):
        print(" ", signal[i], end="")
    print()
    
    print("Processing time:", (now() - start_time) / 1e9, "seconds")
    print("Number of peaks found:", len(peaks)) 