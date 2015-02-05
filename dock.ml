open Batteries

type t = Unix.file_descr

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

let dump prefix data =
  print_string prefix;
  String.to_list data
  |> List.map (fun c -> Printf.sprintf "%02X" (Char.code c))
  |> String.concat " "
  |> print_endline

let format_command ~code ~address ~ans_size =
  let output = IO.output_string () in
  IO.write_byte output code;
  IO.write_ui16 output address;
  IO.write_ui16 output ans_size;
  IO.close_out output

let fully_received ans =
  let n = String.length ans in
  n > 2 && ans.[n - 2] == '\x00' && ans.[n - 1] == '\xFF'


let close = Unix.close

let open_dock path =
  let fd = Unix.(openfile path [O_NONBLOCK; O_RDWR] 0o640) in
  Unix.(tcflush fd TCIOFLUSH);
  Unix.(tcsetattr fd TCSANOW (cfmakeraw (tcgetattr fd)));
  fd


let io_timeout = 5.0

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

let simple_command fd ~code ~ans_size =
  let output = IO.output_string () in
  IO.write_byte output code;
  let data = IO.close_out output in
  write fd data;
  read fd ans_size

let command fd ~code ~address ~ans_size =
  write fd (format_command ~code ~address ~ans_size);
  let ans = read fd (ans_size + 2) in
  if not (fully_received ans) then
    failwith "Missing end of response marker";
  String.sub ans 0 ans_size

let pkg_command fd ~code ~address ~ans_size ~pkg_size =
  let ans = String.make ans_size '?' in
  let rec command_at off =
    if off < ans_size then
      begin
        let n = min (ans_size - off) pkg_size in
        write fd (format_command ~code
                                 ~address:(address + off)
                                 ~ans_size:n);
        let buf = read fd (n + 2) in
        if not (fully_received buf) then
          failwith "Missing end of response marker";
        String.blit buf 0 ans off n;
        command_at (off + n)
      end in
  command_at 0;
  ans

let device_connected fd =
  let ans = simple_command fd ~code:0xF4 ~ans_size:1 in
  ans.[0] == '\x01'

let device_info fd =
  let ans = simple_command fd ~code:0xFE ~ans_size:11 in
  match ans.[1] with
    '\x00' -> None
  | '\x17' ->
     Some { Device_info.model = Device_model.Rox_5;
            serial_number = String.sub ans 2 4 }
  | '\x18' ->
     Some { Device_info.model = Device_model.Rox_6;
            serial_number = String.sub ans 2 4 }
  | _ -> failwith "Unsupported device model"
