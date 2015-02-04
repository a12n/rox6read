open Batteries

type t = Unix.file_descr

let dump prefix data =
  print_string prefix;
  String.to_list data
  |> List.map (fun c -> Printf.sprintf "%02X" (Char.code c))
  |> String.concat " "
  |> print_endline

let cfmakeraw tio =
  Unix.({ tio with
          c_brkint = false;
          c_csize = 8;
          c_echo = false;
          c_echonl = false;
          c_icanon = false;
          c_icrnl = false;
          c_ignbrk = false;
          c_igncr = false;
          c_inlcr = false;
          c_isig = false;
          c_istrip = false;
          c_ixon = false;
          c_opost = false;
          c_parenb = false;
          c_parmrk = false })


let open_dock path =
  let fd = Unix.(openfile path [O_RDWR] 0o640) in
  Unix.(tcflush fd TCIOFLUSH);
  Unix.(tcsetattr fd TCSANOW (cfmakeraw (tcgetattr fd)));
  fd

let close = Unix.close


(** Read [n] bytes of data from the docking station. *)
let read fd n =
  let buf = String.make n '?' in
  ignore (Unix.read fd buf 0 n);
  dump "DS -> " buf;
  buf

(** Write [data] to the docking station. *)
let write fd data =
  dump "DS <- " data;
  let n = String.length data in
  ignore (Unix.write fd data 0 n)

let command fd ~data ~ans_size =
  write fd data;
  read fd ans_size


let device_connected fd =
  let ans = command fd ~data:"\xF4" ~ans_size:1 in
  ans.[0] == '\x01'

let device_model fd =
  let ans = command fd ~data:"\xFE" ~ans_size:11 in
  match ans.[1] with
    '\x00' -> None
  | '\x17' -> Some (Device_model.Rox_5)
  | '\x18' -> Some (Device_model.Rox_6)
  | _ -> failwith "Unsupported device model"
