(*====================================================
OCaml FFT Task Reference
====================================================*)

open Complex

(* Generate signal with given noise *)
let generate_signal n freq noise =
  let dt = 1.0 /. float_of_int n in
  let time_points = List.init n (fun i -> float_of_int i *. dt) in
  let sine_wave = List.map (fun t -> sin (2.0 *. Float.pi *. freq *. t)) time_points in
  List.map2 (+.) sine_wave (List.take n noise)

(* Generate synthetic data with random noise *)
let generate_synthetic_data n freq noise_amp =
  let rng = Random.State.make_self_init () in
  let noise = List.init n (fun _ -> 
    noise_amp *. (2.0 *. Random.State.float rng 1.0 -. 1.0)
  ) in
  generate_signal n freq noise

(* Moving average filter *)
let moving_average_filter window_size signal =
  if window_size <= 0 then signal
  else
    let rec sliding_windows acc i =
      if i > List.length signal - window_size then
        List.rev acc
      else
        let window = List.take window_size (List.drop i signal) in
        sliding_windows ((List.fold_left (+.) 0.0 window /. float_of_int window_size) :: acc) (i + 1)
    in
    sliding_windows [] 0

(* Basic DFT implementation *)
let basic_dft signal =
  let n = List.length signal in
  let omega k n' =
    let theta = -2.0 *. Float.pi *. float_of_int (k * n') /. float_of_int n in
    {re = cos theta; im = sin theta}
  in
  List.init n (fun k ->
    List.fold_left2
      (fun acc x n' ->
        let w = omega k n' in
        add acc (scalar_mul x w))
      {re = 0.0; im = 0.0}
      signal
      (List.init n (fun i -> i)))

(* Find peaks above threshold *)
let find_peaks threshold spectrum =
  let magnitude z = sqrt (z.re *. z.re +. z.im *. z.im) in
  let is_peak i val =
    i > 0 && i < List.length spectrum - 1 &&
    magnitude val > magnitude (List.nth spectrum (i-1)) &&
    magnitude val > magnitude (List.nth spectrum (i+1))
  in
  List.filter_mapi
    (fun i val ->
      if magnitude val > threshold && is_peak i val then
        Some i
      else None)
    spectrum

(* Main function *)
let () =
  let samples = generate_synthetic_data 10000 10.0 0.1 in
  List.take 10 samples |> List.iter (Printf.printf "%.6f ")
