use rand::Rng;
use std::f64::consts::PI;
use num_complex::Complex64;

/// Generates a sine wave signal with added noise
fn generate_signal(n: usize, freq: f64, noise: &[f64]) -> Vec<f64> {
    let dt = 1.0 / n as f64;
    
    (0..n)
        .map(|i| {
            let t = i as f64 * dt;
            let sine = (2.0 * PI * freq * t).sin();
            sine + noise[i]
        })
        .collect()
}

/// Generates synthetic data with random noise
fn generate_synthetic_data(n: usize, freq: f64, noise_amp: f64) -> Vec<f64> {
    let mut rng = rand::thread_rng();
    let noise: Vec<f64> = (0..n)
        .map(|_| rng.gen_range(-noise_amp..noise_amp))
        .collect();
    
    generate_signal(n, freq, &noise)
}

/// Applies a moving average filter to the signal
fn moving_average_filter(window_size: usize, signal: &[f64]) -> Vec<f64> {
    if window_size <= 0 {
        return signal.to_vec();
    }

    (0..=signal.len() - window_size)
        .map(|i| {
            let window = &signal[i..i + window_size];
            window.iter().sum::<f64>() / window_size as f64
        })
        .collect()
}

/// Computes the basic DFT of the signal
fn basic_dft(signal: &[f64]) -> Vec<Complex64> {
    let n = signal.len();
    
    (0..n)
        .map(|k| {
            (0..n)
                .map(|n| {
                    let x = signal[n];
                    let omega = Complex64::from_polar(
                        1.0,
                        -2.0 * PI * (k * n) as f64 / signal.len() as f64
                    );
                    omega * x
                })
                .sum()
        })
        .collect()
}

/// Finds peaks in the spectrum above the threshold
fn find_peaks(threshold: f64, spectrum: &[Complex64]) -> Vec<usize> {
    (1..spectrum.len() - 1)
        .filter(|&i| {
            let mag = spectrum[i].norm();
            mag > threshold &&
            mag > spectrum[i - 1].norm() &&
            mag > spectrum[i + 1].norm()
        })
        .collect()
}

fn main() {
    // Generate 10,000 samples with frequency 10Hz and noise amplitude 0.1
    let samples = generate_synthetic_data(10_000, 10.0, 0.1);
    
    // Print first 10 samples
    println!("First 10 samples: {:?}", &samples[..10]);
    
    // Example of full pipeline
    let filtered = moving_average_filter(50, &samples);
    let spectrum = basic_dft(&filtered);
    let peaks = find_peaks(1.0, &spectrum);
    
    println!("Found peaks at indices: {:?}", peaks);
}