//====================================================
// C++ FFT Task Reference
//====================================================

#include <vector>
#include <complex>
#include <cmath>

class FFTTask {
private:
    const double PI = 3.14159265358979323846;

    // Helper function to check if number is power of 2
    bool isPowerOfTwo(int n) {
        return n > 0 && (n & (n - 1)) == 0;
    }

    // Bit reversal for FFT
    int bitReverse(int x, int log2n) {
        int n = 0;
        for (int i = 0; i < log2n; i++) {
            n = (n << 1) | (x & 1);
            x >>= 1;
        }
        return n;
    }

public:
    // Forward FFT implementation
    std::vector<std::complex<double>> fft(const std::vector<std::complex<double>>& input) {
        int n = input.size();
        
        // Check if input size is power of 2
        if (!isPowerOfTwo(n)) {
            throw std::runtime_error("Input size must be a power of 2");
        }

        std::vector<std::complex<double>> output(n);
        int log2n = static_cast<int>(log2(n));

        // Bit reversal
        for (int i = 0; i < n; i++) {
            output[bitReverse(i, log2n)] = input[i];
        }

        // FFT computation
        for (int s = 1; s <= log2n; s++) {
            int m = 1 << s;  // 2^s
            int halfm = m >> 1;
            std::complex<double> omega(cos(2 * PI / m), sin(2 * PI / m));
            
            for (int k = 0; k < n; k += m) {
                std::complex<double> w(1, 0);
                for (int j = 0; j < halfm; j++) {
                    std::complex<double> t = w * output[k + j + halfm];
                    std::complex<double> u = output[k + j];
                    output[k + j] = u + t;
                    output[k + j + halfm] = u - t;
                    w *= omega;
                }
            }
        }

        return output;
    }

    // Inverse FFT implementation
    std::vector<std::complex<double>> ifft(const std::vector<std::complex<double>>& input) {
        int n = input.size();
        std::vector<std::complex<double>> conjugate_input(n);
        
        // Take complex conjugate
        for (int i = 0; i < n; i++) {
            conjugate_input[i] = std::conj(input[i]);
        }

        // Perform forward FFT
        auto result = fft(conjugate_input);

        // Take complex conjugate and scale
        for (int i = 0; i < n; i++) {
            result[i] = std::conj(result[i]) / static_cast<double>(n);
        }

        return result;
    }
};
