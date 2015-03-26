open Batteries

module Fft = Fftw3.D

let xcorr a b =
  let n_in = max (Array.length a) (Array.length b) in
  let max_lag = n_in - 1 in
  let n_out = 2 * max_lag + 1 in
  let n_freq = n_out / 2 + 1 in
  let x = Fft.Array1.create Fft.float Bigarray.c_layout n_out in
  let a' = Fft.Array1.create Fft.complex Bigarray.c_layout n_freq in
  let b' = Fft.Array1.create Fft.complex Bigarray.c_layout n_freq in
  let fft_a = Fft.Array1.r2c x a' in
  let fft_b = Fft.Array1.r2c x b' in
  let ifft = Fft.Array1.c2r a' x in
  (* DFT 1 *)
  Bigarray.Array1.modifyi (fun k _ -> try a.(k - n_in + 1)
                                      with Invalid_argument _ -> 0.0) x;
  Fft.exec fft_a;
  (* DFT 2 *)
  Bigarray.Array1.modifyi (fun k _ -> try b.(k)
                                      with Invalid_argument _ -> 0.0) x;
  Fft.exec fft_b;
  (* Correlation, IDFT *)
  Bigarray.Array1.modifyi (fun k _ -> Complex.(mul a'.{k} (conj b'.{k}))) a';
  Fft.exec ifft;
  (* Prepare output *)
  let n_out_rcp = 1.0 /. float_of_int n_out in
  Array.init n_out (fun k -> k - max_lag, x.{k} *. n_out_rcp)

let max_xcorr a b =
  xcorr a b |> Array.reduce (fun r1 r2 -> if (snd r1) > (snd r2) then r1 else r2)
