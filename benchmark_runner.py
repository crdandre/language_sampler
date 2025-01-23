"""
For Haskell
python benchmark_runner.py haskell/fft_task.hs --language haskell

For Python
python benchmark_runner.py fft_task.py --language python

For Julia
python benchmark_runner.py fft_task.jl --language julia

TODO: compiled vs interpreted haskell?
"""


import subprocess
import time
import psutil
import statistics
from datetime import datetime
import json
import os
from typing import Dict, List, Optional
from dataclasses import dataclass
import argparse

@dataclass
class LanguageConfig:
    name: str
    file_extension: str
    compile_cmd: Optional[List[str]] = None  # None for interpreted languages
    run_cmd: List[str] = None
    cleanup_files: List[str] = None  # Files to cleanup after benchmarking

LANGUAGE_CONFIGS = {
    "python": LanguageConfig(
        name="Python",
        file_extension=".py",
        run_cmd=["python3", "{filename}"]
    ),
    "haskell": LanguageConfig(
        name="Haskell",
        file_extension=".hs",
        run_cmd=["runghc", "{filename}"]  # Simplified to use runghc
    ),
    "rust": LanguageConfig(
        name="Rust",
        file_extension=".rs",
        compile_cmd=["rustc", "{filename}"],
        run_cmd=["./{basename}"],
        cleanup_files=["{basename}"]
    ),
    "go": LanguageConfig(
        name="Go",
        file_extension=".go",
        compile_cmd=["go", "build", "{filename}"],
        run_cmd=["./{basename}"],
        cleanup_files=["{basename}"]
    ),
    "julia": LanguageConfig(
        name="Julia",
        file_extension=".jl",
        run_cmd=["julia", "{filename}"]
    ),
    # Add more languages as needed
}

class BenchmarkRunner:
    def __init__(self, filename: str, language: str, num_runs: int = 10):
        self.filename = os.path.normpath(filename)  # Normalize the path
        # Get just the basename without any directory components
        self.basename = os.path.splitext(os.path.basename(self.filename))[0]
        self.num_runs = num_runs
        
        if language not in LANGUAGE_CONFIGS:
            raise ValueError(f"Unsupported language: {language}")
        
        self.config = LANGUAGE_CONFIGS[language]
        self.metrics = {
            "language": self.config.name,
            "filename": filename,
            "timestamp": datetime.now().isoformat(),
            "runs": [],
            "memory_usage": [],
            "average_metrics": {}
        }

    def compile_if_needed(self):
        """Compile the program if it's a compiled language."""
        if self.config.compile_cmd:
            cmd = [arg.format(filename=self.filename, basename=self.basename) 
                  for arg in self.config.compile_cmd]
            print(f"Compiling with command: {' '.join(cmd)}")
            subprocess.run(cmd, check=True)

    def run_single_benchmark(self, run_number: int) -> Dict:
        """Run a single benchmark iteration."""
        process = psutil.Process()
        start_memory = process.memory_info().rss / 1024 / 1024  # MB

        cmd = [arg.format(filename=self.filename, basename=self.basename) 
               for arg in self.config.run_cmd]
        
        start_time = time.time()
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        end_time = time.time()

        end_memory = process.memory_info().rss / 1024 / 1024  # MB

        return {
            "run_number": run_number,
            "total_time": end_time - start_time,
            "memory_mb": end_memory - start_memory,
            "output": result.stdout.strip()
        }

    def cleanup(self):
        """Clean up compiled files if any."""
        if self.config.cleanup_files:
            for pattern in self.config.cleanup_files:
                try:
                    filename = pattern.format(filename=self.filename, basename=self.basename)
                    if os.path.exists(filename):
                        os.remove(filename)
                except Exception as e:
                    print(f"Warning: Cleanup failed for {filename}: {e}")

    def run_benchmarks(self):
        """Run all benchmarks and collect metrics."""
        try:
            self.compile_if_needed()

            for i in range(self.num_runs):
                print(f"Run {i+1}/{self.num_runs}")
                run_data = self.run_single_benchmark(i + 1)
                self.metrics["runs"].append(run_data)
                self.metrics["memory_usage"].append(run_data["memory_mb"])

            # Calculate statistics
            times = [r["total_time"] for r in self.metrics["runs"]]
            self.metrics["average_metrics"] = {
                "avg_execution_time": statistics.mean(times),
                "std_execution_time": statistics.stdev(times),
                "avg_memory_usage": statistics.mean(self.metrics["memory_usage"]),
                "min_time": min(times),
                "max_time": max(times)
            }

            # Create the output filename using just the basename
            output_file = f"benchmark_results_{self.basename}.json"
            with open(output_file, "w") as f:
                json.dump(self.metrics, f, indent=2)

            self.print_summary()

        finally:
            self.cleanup()

    def print_summary(self):
        """Print benchmark summary."""
        print("\nBenchmark Summary:")
        print(f"Language: {self.config.name}")
        print(f"File: {self.filename}")
        print(f"Average execution time: {self.metrics['average_metrics']['avg_execution_time']:.4f} seconds")
        print(f"Standard deviation: {self.metrics['average_metrics']['std_execution_time']:.4f} seconds")
        print(f"Average memory usage: {self.metrics['average_metrics']['avg_memory_usage']:.2f} MB")
        print(f"Results saved to: benchmark_results_{self.basename}.json")

def main():
    parser = argparse.ArgumentParser(description='Generic benchmark runner for various programming languages')
    parser.add_argument('filename', help='Source file to benchmark')
    parser.add_argument('--language', required=True, choices=LANGUAGE_CONFIGS.keys(),
                      help='Programming language of the source file')
    parser.add_argument('--runs', type=int, default=3,
                      help='Number of benchmark runs (default: 3, must be >1)')

    args = parser.parse_args()
    
    runner = BenchmarkRunner(args.filename, args.language, args.runs)
    runner.run_benchmarks()

if __name__ == "__main__":
    main()
