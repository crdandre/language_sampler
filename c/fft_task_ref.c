//====================================================
// C FFT Task Reference
// gcc -o fft_task fft_task_ref.c -lm
//====================================================

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <complex.h>

#define N 10000        // Number of samples
#define FREQ 10.0      // Signal frequency
#define NOISE_AMP 0.1  // Noise amplitude
#define WINDOW_SIZE 50 // Moving average window size

// Generate synthetic signal: sin(2πft) + noise
void generate_synthetic_data(double* signal) {
    srand(time(NULL));
    double dt = 1.0 / N;
    
    for (int i = 0; i < N; i++) {
        double t = i * dt;
        double noise = ((double)rand() / RAND_MAX) * 2 * NOISE_AMP - NOISE_AMP;
        signal[i] = sin(2 * M_PI * FREQ * t) + noise;
    }
}

// Apply moving average filter
void moving_average_filter(const double* input, double* output, int window_size) {
    if (window_size <= 0) {
        memcpy(output, input, N * sizeof(double));
        return;
    }

    for (int i = 0; i <= N - window_size; i++) {
        double sum = 0.0;
        for (int j = 0; j < window_size; j++) {
            sum += input[i + j];
        }
        output[i] = sum / window_size;
    }
}

// Compute basic DFT
void compute_dft(const double* signal, double complex* spectrum) {
    for (int k = 0; k < N; k++) {
        spectrum[k] = 0;
        for (int n = 0; n < N; n++) {
            double angle = -2.0 * M_PI * k * n / N;
            spectrum[k] += signal[n] * cexp(I * angle);
        }
    }
}

// Find peaks above threshold
void find_peaks(double complex* spectrum, double threshold) {
    printf("Peaks found at frequencies:\n");
    for (int i = 1; i < N-1; i++) {
        double magnitude = cabs(spectrum[i]);
        double prev_mag = cabs(spectrum[i-1]);
        double next_mag = cabs(spectrum[i+1]);
        
        if (magnitude > threshold && 
            magnitude > prev_mag && 
            magnitude > next_mag) {
            // Convert index to frequency
            double freq = (double)i * FREQ / N;
            printf("%.2f Hz (magnitude: %.2f)\n", freq, magnitude);
        }
    }
}

int main() {
    // Allocate memory
    double* signal = (double*)malloc(N * sizeof(double));
    double* filtered_signal = (double*)malloc(N * sizeof(double));
    double complex* spectrum = (double complex*)malloc(N * sizeof(double complex));

    if (!signal || !filtered_signal || !spectrum) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    // Execute pipeline
    generate_synthetic_data(signal);
    
    printf("First 10 samples of raw signal:\n");
    for (int i = 0; i < 10; i++) {
        printf("%.4f ", signal[i]);
    }
    printf("\n\n");

    moving_average_filter(signal, filtered_signal, WINDOW_SIZE);
    compute_dft(filtered_signal, spectrum);
    find_peaks(spectrum, 100.0);  // Threshold may need adjustment

    // Clean up
    free(signal);
    free(filtered_signal);
    free(spectrum);

    return 0;
} 