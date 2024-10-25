# Language Sampler

## Project: Signal Processing Pipeline
Each implementation should:
1. Generate 10,000 samples of synthetic data: y(t) = sin(2πft) + noise
2. Apply a moving average filter (window size = 50)
3. Compute FFT
4. Find peaks above threshold
5. Output timing metrics and results

### Languages (Ordered High to Low Level):
- **Julia** [High-level, Multiple Paradigm: Functional, OOP, Procedural]
  - Mathematical expressiveness, array operations
- **Clojure** [High-level, Functional, LISP dialect]
  - Data transformation pipelines
- **Elixir** [High-level, Functional]
  - Actor model, distributed processing
- **Python/Bend** [High-level, Multiple Paradigm: OOP, Procedural, Functional]
  - "CUDA in python wrapping?" parallel programming with simple syntax
- **OCaml** [High-level, Functional with OOP support]
  - Pattern matching, type inference
- **Haskell** [High-level, Pure Functional]
  - Pure functional approach, lazy evaluation
- **Mojo** [High-level, Python Superset with Systems Programming Capabilities]
  - MLIR-based compiler for heterogeneous hardware (CPU/GPU/AI ASICs)
  - Python-compatible with systems programming performance
- **Go** [Low-level, Procedural with CSP concurrency]
  - Concurrent processing, clean syntax
- **Rust** [Low-level, Multi-paradigm with focus on Systems Programming]
  - Memory safety, SIMD optimizations
- **C/C++** [Low-level, Multi-paradigm: OOP, Procedural]
  - Raw performance, direct memory control
- **Zig** [Low-level, Procedural]
  - Low-level control, compile-time features
- **CUDA** [Low-level, Parallel Computing]
  - Parallel processing on GPU
- **Assembly** [Low-level]
  - Direct hardware interaction

## Comparison Metrics:
1. Code length and readability:
   - Lines of code (excluding comments and blank lines)
   - Cyclomatic complexity score
   - Halstead complexity measures (program length, vocabulary, volume, difficulty)

2. Execution speed:
   - Wall clock time for complete pipeline execution (average of 10 runs)
   - CPU time for each pipeline stage (in milliseconds)
   - Throughput: samples processed per second

3. Memory usage:
   - Peak memory consumption (in MB)
   - Memory usage over time (sampled every 100ms)
   - Garbage collection frequency and duration (if applicable)

4. Development time:
   - Time to implement basic functionality (in person-hours)
   - Time to optimize for performance (in person-hours)
   - Learning curve rating (1-10 scale, 1 being easiest)

5. Parallel processing capabilities:
   - Speedup factor when using multiple cores (2, 4, 8 cores)
   - Scalability: performance change from 1,000 to 1,000,000 samples
   - Ease of parallelization implementation (1-10 scale, 10 being easiest)
