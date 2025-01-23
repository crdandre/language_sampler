#====================================================
# Elixir FFT Task Reference
#====================================================

defmodule FFTTask do
  @moduledoc """
  Signal processing pipeline that:
  1. Generates 10,000 samples of synthetic data: y(t) = sin(2πft) + noise
  2. Applies a moving average filter (window size = 50)
  3. Computes FFT
  4. Finds peaks above threshold
  5. Outputs results
  """

  @doc """
  Generates a sine wave signal with added noise
  """
  def generate_signal(n, freq, noise) do
    dt = 1.0 / n
    time_points = for i <- 0..(n-1), do: i * dt

    sine_wave = for t <- time_points, do: :math.sin(2 * :math.pi * freq * t)
    Enum.zip_with(sine_wave, Enum.take(noise, n), fn x, y -> x + y end)
  end

  @doc """
  Generates synthetic data with random noise
  """
  def generate_synthetic_data(n, freq, noise_amp) do
    noise = for _i <- 1..n do
      :rand.uniform() * 2 * noise_amp - noise_amp
    end
    generate_signal(n, freq, noise)
  end

  @doc """
  Applies a moving average filter to the signal
  """
  def moving_average_filter(_signal, window_size) when window_size <= 0, do: raise "Invalid window size"
  def moving_average_filter(signal, window_size) do
    signal
    |> Enum.chunk_every(window_size, 1, :discard)
    |> Enum.map(fn window -> Enum.sum(window) / window_size end)
  end

  @doc """
  Computes the basic DFT (Discrete Fourier Transform)
  """
  def basic_dft(signal) do
    n = length(signal)
    for k <- 0..(n-1) do
      Enum.zip(signal, 0..(n-1))
      |> Enum.map(fn {x, n} ->
        omega = :math.pi * -2 * k * n / length(signal)
        x * Complex.new(:math.cos(omega), :math.sin(omega))
      end)
      |> Enum.reduce(&Complex.add/2)
    end
  end

  @doc """
  Finds peaks in the spectrum above a given threshold
  """
  def find_peaks(spectrum, threshold) do
    spectrum
    |> Enum.with_index()
    |> Enum.filter(fn {val, i} ->
      magnitude = Complex.abs(val)
      i > 0 && i < length(spectrum) - 1 &&
      magnitude > threshold &&
      magnitude > Complex.abs(Enum.at(spectrum, i-1)) &&
      magnitude > Complex.abs(Enum.at(spectrum, i+1))
    end)
    |> Enum.map(fn {_val, i} -> i end)
  end

  def run do
    n = 10_000
    freq = 10.0
    noise_amp = 0.1
    window_size = 50
    threshold = 1000.0

    # Execute pipeline
    signal = generate_synthetic_data(n, freq, noise_amp)
    filtered = moving_average_filter(signal, window_size)
    spectrum = basic_dft(filtered)
    peaks = find_peaks(spectrum, threshold)

    # Print first 10 samples for verification
    IO.puts "First 10 samples:"
    signal |> Enum.take(10) |> IO.inspect()

    IO.puts "\nPeak indices:"
    IO.inspect(peaks)
  end
end

# You'll need to add this to mix.exs dependencies:
# {:complex, "~> 0.4"}
