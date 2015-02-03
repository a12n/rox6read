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
  buf

(** Write [data] to the docking station. *)
let write fd data =
  let n = String.length data in
  ignore (Unix.write fd data 0 n)

let command fd data n =
  write fd data;
  read fd n


let unit_connected fd =
  (command fd "\xF4" 1).[0] == '\x01'

let unit_model fd =
  match (command fd "\xFE" 11).[1] with
    '\x00' -> None
  | '\x17' -> Some (Unit_model.Rox_5)
  | '\x18' -> Some (Unit_model.Rox_6)
  | model -> Some (Unit_model.Other (Char.code model))