//====================================================
// Zig FFT Task Reference
//====================================================

const std = @import("std");
const math = std.math;
const Complex = std.math.Complex(f64);
const ArrayList = std.ArrayList;
const Random = std.rand.DefaultPrng;

// Generate synthetic signal: sin(2πft) + noise
fn generateSignal(allocator: std.mem.Allocator, n: usize, freq: f64, noise: []const f64) ![]f64 {
    var signal = try allocator.alloc(f64, n);
    const dt = 1.0 / @as(f64, @floatFromInt(n));
    
    for (0..n) |i| {
        const t = dt * @as(f64, @floatFromInt(i));
        signal[i] = math.sin(2.0 * math.pi * freq * t) + noise[i];
    }
    return signal;
}

// Generate random noise
fn generateNoise(allocator: std.mem.Allocator, n: usize, noise_amp: f64) ![]f64 {
    var rng = Random.init(0);
    var noise = try allocator.alloc(f64, n);
    
    for (noise) |*val| {
        val.* = (rng.random().float(f64) * 2.0 - 1.0) * noise_amp;
    }
    return noise;
}

// Moving average filter
fn movingAverageFilter(allocator: std.mem.Allocator, signal: []const f64, window_size: usize) ![]f64 {
    if (window_size <= 0 or window_size > signal.len) return allocator.dupe(f64, signal);
    
    const result_len = signal.len - window_size + 1;
    var filtered = try allocator.alloc(f64, result_len);
    
    for (0..result_len) |i| {
        var sum: f64 = 0;
        for (0..window_size) |j| {
            sum += signal[i + j];
        }
        filtered[i] = sum / @as(f64, @floatFromInt(window_size));
    }
    return filtered;
}

// Basic DFT implementation
fn basicDFT(allocator: std.mem.Allocator, signal: []const f64) ![]Complex {
    const n = signal.len;
    var spectrum = try allocator.alloc(Complex, n);
    
    for (0..n) |k| {
        var sum = Complex.init(0, 0);
        for (0..n) |j| {
            const angle = -2.0 * math.pi * @as(f64, @floatFromInt(k * j)) / @as(f64, @floatFromInt(n));
            const omega = Complex.init(math.cos(angle), math.sin(angle));
            sum = sum.add(omega.mul(Complex.init(signal[j], 0)));
        }
        spectrum[k] = sum;
    }
    return spectrum;
}

// Find peaks above threshold
fn findPeaks(allocator: std.mem.Allocator, spectrum: []const Complex, threshold: f64) ![]usize {
    var peaks = ArrayList(usize).init(allocator);
    
    for (1..spectrum.len - 1) |i| {
        const mag = spectrum[i].magnitude();
        if (mag > threshold and
            mag > spectrum[i-1].magnitude() and
            mag > spectrum[i+1].magnitude()) {
            try peaks.append(i);
        }
    }
    return peaks.toOwnedSlice();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    
    // Parameters
    const n: usize = 10000;
    const freq: f64 = 10.0;
    const noise_amp: f64 = 0.1;
    const window_size: usize = 50;
    const peak_threshold: f64 = 1000.0; // Adjust based on your needs
    
    // Generate synthetic data
    const noise = try generateNoise(allocator, n, noise_amp);
    defer allocator.free(noise);
    
    const signal = try generateSignal(allocator, n, freq, noise);
    defer allocator.free(signal);
    
    // Apply moving average filter
    const filtered = try movingAverageFilter(allocator, signal, window_size);
    defer allocator.free(filtered);
    
    // Compute FFT
    const spectrum = try basicDFT(allocator, filtered);
    defer allocator.free(spectrum);
    
    // Find peaks
    const peaks = try findPeaks(allocator, spectrum, peak_threshold);
    defer allocator.free(peaks);
    
    // Print first 10 samples (like in Haskell version)
    std.debug.print("First 10 samples:\n", .{});
    for (signal[0..10]) |sample| {
        std.debug.print("{d:.6}\n", .{sample});
    }
    
    std.debug.print("\nFound {} peaks\n", .{peaks.len});
}
