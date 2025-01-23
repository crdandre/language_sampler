//====================================================
// CUDA FFT Task Reference
//====================================================

#include <cuda_runtime.h>
#include <cufft.h>
#include <curand.h>
#include <stdio.h>
#include <math.h>

// Constants
#define BLOCK_SIZE 256
#define SIGNAL_LENGTH 1024
#define WINDOW_SIZE 8
#define PI 3.14159265359f

// Error checking macro
#define cudaCheckError(ans) { gpuAssert((ans), __FILE__, __LINE__); }
inline void gpuAssert(cudaError_t code, const char *file, int line) {
    if (code != cudaSuccess) {
        fprintf(stderr,"GPUassert: %s %s %d\n", cudaGetErrorString(code), file, line);
        exit(code);
    }
}

// Kernel for generating sine wave
__global__ void generateSignalKernel(float* signal, float frequency, float sampleRate) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < SIGNAL_LENGTH) {
        float t = idx / sampleRate;
        signal[idx] = sinf(2.0f * PI * frequency * t);
    }
}

// Kernel for moving average filter
__global__ void movingAverageKernel(float* input, float* output) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    
    if (idx < SIGNAL_LENGTH - WINDOW_SIZE + 1) {
        float sum = 0.0f;
        for (int i = 0; i < WINDOW_SIZE; i++) {
            sum += input[idx + i];
        }
        output[idx] = sum / WINDOW_SIZE;
    }
}

// Kernel for peak finding
__global__ void findPeaksKernel(float* signal, int* peaks, float threshold) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    
    if (idx > 0 && idx < SIGNAL_LENGTH - 1) {
        if (signal[idx] > threshold &&
            signal[idx] > signal[idx-1] &&
            signal[idx] > signal[idx+1]) {
            peaks[idx] = 1;
        } else {
            peaks[idx] = 0;
        }
    }
}

int main() {
    // Host arrays
    float *h_signal = nullptr;
    float *h_filtered = nullptr;
    float *h_spectrum = nullptr;
    int *h_peaks = nullptr;

    // Device arrays
    float *d_signal = nullptr;
    float *d_filtered = nullptr;
    float *d_spectrum = nullptr;
    int *d_peaks = nullptr;

    // Allocate host memory
    h_signal = (float*)malloc(SIGNAL_LENGTH * sizeof(float));
    h_filtered = (float*)malloc(SIGNAL_LENGTH * sizeof(float));
    h_spectrum = (float*)malloc(SIGNAL_LENGTH * sizeof(float));
    h_peaks = (int*)malloc(SIGNAL_LENGTH * sizeof(int));

    // Allocate device memory
    cudaCheckError(cudaMalloc(&d_signal, SIGNAL_LENGTH * sizeof(float)));
    cudaCheckError(cudaMalloc(&d_filtered, SIGNAL_LENGTH * sizeof(float)));
    cudaCheckError(cudaMalloc(&d_spectrum, SIGNAL_LENGTH * sizeof(float)));
    cudaCheckError(cudaMalloc(&d_peaks, SIGNAL_LENGTH * sizeof(int)));

    // Calculate grid and block dimensions
    dim3 blockDim(BLOCK_SIZE);
    dim3 gridDim((SIGNAL_LENGTH + BLOCK_SIZE - 1) / BLOCK_SIZE);

    // Generate signal
    generateSignalKernel<<<gridDim, blockDim>>>(d_signal, 10.0f, 1000.0f);
    cudaCheckError(cudaGetLastError());

    // Apply moving average filter
    movingAverageKernel<<<gridDim, blockDim>>>(d_signal, d_filtered);
    cudaCheckError(cudaGetLastError());

    // Setup cuFFT
    cufftHandle plan;
    cufftPlan1d(&plan, SIGNAL_LENGTH, CUFFT_R2C, 1);
    cufftExecR2C(plan, (cufftReal*)d_filtered, (cufftComplex*)d_spectrum);
    
    // Find peaks
    findPeaksKernel<<<gridDim, blockDim>>>(d_spectrum, d_peaks, 0.5f);
    cudaCheckError(cudaGetLastError());

    // Copy results back to host
    cudaCheckError(cudaMemcpy(h_signal, d_signal, SIGNAL_LENGTH * sizeof(float), cudaMemcpyDeviceToHost));
    cudaCheckError(cudaMemcpy(h_filtered, d_filtered, SIGNAL_LENGTH * sizeof(float), cudaMemcpyDeviceToHost));
    cudaCheckError(cudaMemcpy(h_spectrum, d_spectrum, SIGNAL_LENGTH * sizeof(float), cudaMemcpyDeviceToHost));
    cudaCheckError(cudaMemcpy(h_peaks, d_peaks, SIGNAL_LENGTH * sizeof(int), cudaMemcpyDeviceToHost));

    // Cleanup
    cufftDestroy(plan);
    cudaFree(d_signal);
    cudaFree(d_filtered);
    cudaFree(d_spectrum);
    cudaFree(d_peaks);
    free(h_signal);
    free(h_filtered);
    free(h_spectrum);
    free(h_peaks);

    return 0;
}
