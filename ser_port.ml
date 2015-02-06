type t = Unix.file_descr

exception Timeout

let dump prefix buf =
  prerr_string prefix;
  String.iter (fun c -> Printf.fprintf stderr " %02X" (Char.code c)) buf;
  prerr_newline ()

let cfmakeraw attr =
  { attr with Unix.c_brkint = false; c_csize = 8; c_echo = false;
              c_echonl = false; c_icanon = false; c_icrnl = false;
              c_ignbrk = false; c_igncr = false; c_inlcr = false;
              c_isig = false; c_istrip = false; c_ixon = false;
              c_opost = false; c_parenb = false; c_parmrk = false }

let close = Unix.close

let open_port path =
  let fd = Unix.(openfile path [O_NONBLOCK; O_RDWR] 0o640) in
  Unix.(tcflush fd TCIOFLUSH);
  (* TODO: Set speed *)
  Unix.(tcsetattr fd TCSANOW (cfmakeraw (tcgetattr fd)));
  fd

let timeout = 5.0

let read fd n =
  (* TODO: use bytes *)
  let buf = String.make n '\x00' in
  let rec aux k =
    if k < n then
      match Unix.select [fd] [] [] timeout with
      | [fd], [], [] ->
         let m = Unix.read fd buf k (n - k) in
         dump "PORT -->" (String.sub buf k m);
         aux (k + m)
      | _ -> raise Timeout
    else
      buf in
  aux 0

let write fd buf =
  (* TODO: use bytes *)
  let n = String.length buf in
  let rec aux k =
    if k < n then
      match Unix.select [] [fd] [] timeout with
      | [], [fd], [] ->
         let m = Unix.write fd buf k (n - k) in
         dump "PORT <--" (String.sub buf k m);
         aux (k + m)
      | _ -> raise Timeout in
  aux 0
