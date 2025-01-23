"""
Julia FFT Task Reference
"""

"""
This program implements a signal processing pipeline:
1. Generate 10,000 samples of synthetic data: y(t) = sin(2πft) + noise
2. Apply a moving average filter (window size = 50)
3. Compute FFT
4. Find peaks above threshold
5. Output timing metrics and results in README.md
"""

using Random

"""
Generate synthetic signal with noise
"""
function generate_signal(n::Int, freq::Float64, noise::Vector{Float64})
    dt = 1.0 / n
    time_points = range(0, step=dt, length=n)
    sine_wave = sin.(2 * π * freq .* time_points)
    return sine_wave + noise[1:n]
end

"""
Generate synthetic data with random noise
"""
function generate_synthetic_data(n::Int, freq::Float64, noise_amp::Float64)
    noise = (2 * rand(n) .- 1) .* noise_amp
    return generate_signal(n, freq, noise)
end

"""
Apply moving average filter to signal
"""
function moving_average_filter(window_size::Int, signal::Vector{Float64})
    window_size <= 0 && return signal
    
    n = length(signal)
    filtered = zeros(n - window_size + 1)
    
    # Calculate first window sum
    window_sum = sum(signal[1:window_size])
    filtered[1] = window_sum / window_size
    
    # Sliding window for remaining points
    for i in 2:(n - window_size + 1)
        window_sum = window_sum - signal[i-1] + signal[i+window_size-1]
        filtered[i] = window_sum / window_size
    end
    
    return filtered
end

"""
Basic DFT implementation
"""
function basic_dft(signal::Vector{Float64})
    n = length(signal)
    spectrum = zeros(Complex{Float64}, n)
    
    for k in 0:(n-1)
        for j in 0:(n-1)
            omega = exp(-2π * im * k * j / n)
            spectrum[k+1] += signal[j+1] * omega
        end
    end
    
    return spectrum
end

"""
Find peaks above threshold in spectrum
"""
function find_peaks(threshold::Float64, spectrum::Vector{Complex{Float64}})
    n = length(spectrum)
    peaks = Int[]
    
    for i in 2:(n-1)
        mag = abs(spectrum[i])
        if mag > threshold &&
           mag > abs(spectrum[i-1]) &&
           mag > abs(spectrum[i+1])
            push!(peaks, i-1)  # 0-based indexing for consistency
        end
    end
    
    return peaks
end

function main()
    # Generate synthetic data
    samples = generate_synthetic_data(10000, 10.0, 0.1)
    println("First 10 samples: ", samples[1:10])
    
    # Example pipeline usage:
    # filtered_signal = moving_average_filter(50, samples)
    # spectrum = basic_dft(filtered_signal)
    # peaks = find_peaks(1.0, spectrum)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end 