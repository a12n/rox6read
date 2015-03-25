open Batteries

type t = int array

let of_bytes buf =
  let ans = Array.make (Bytes.length buf) 0 in
  Bytes.iteri (fun i c -> ans.(i) <- Char.code c) buf;
  ans
