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
  let fd = Unix.(openfile path [O_NONBLOCK; O_RDWR] 0o640) in
  Unix.(tcflush fd TCIOFLUSH);
  Unix.(tcsetattr fd TCSANOW (cfmakeraw (tcgetattr fd)));
  fd

let close = Unix.close


let io_timeout = 5.0

(** Read [n] bytes of data from the docking station. *)
let read fd n =
  let buf = String.make n '?' in
  let rec read_at k =
    if k < n then
      match Unix.select [fd] [] [] io_timeout with
      | [fd], [], [] ->
         let m = Unix.read fd buf k (n - k) in
         dump "DS -> " (String.sub buf k m);
         read_at (k + m)
      | _ -> failwith "Read from docking station timed out"
    else
      buf in
  read_at 0

(** Write [data] to the docking station. *)
let write fd data =
  let n = String.length data in
  let rec write_at k =
    if k < n then
      match Unix.select [] [fd] [] io_timeout with
      | [], [fd], [] ->
         let m = Unix.write fd data k (n - k) in
         dump "DS <- " (String.sub data k m);
         write_at (k + m)
      | _ -> failwith "Write to docking station timed out" in
  write_at 0

let command fd ~data ~ans_size =
  write fd data;
  read fd ans_size


let device_connected fd =
  let ans = command fd ~data:"\xF4" ~ans_size:1 in
  ans.[0] == '\x01'

let device_info fd =
  let ans = command fd ~data:"\xFE" ~ans_size:11 in
  match ans.[1] with
    '\x00' -> None
  | '\x17' ->
     Some { Device_info.model = Device_model.Rox_5;
            serial_number = String.sub ans 2 4 }
  | '\x18' ->
     Some { Device_info.model = Device_model.Rox_6;
            serial_number = String.sub ans 2 4 }
  | _ -> failwith "Unsupported device model"
