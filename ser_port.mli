(** Serial port connection to the docking station. *)

(** Serial port state. *)
type t

(** Operation is timed out *)
exception Timeout

(** Close serial port. *)
val close : t -> unit

(** Open serial device file at [path]. *)
val open_port : string -> t

(** Read exactly [n] bytes from the port, or fail with [Timeout]. *)
val read : t -> int -> string

(** Write data from [buf] to the port, or fail with [Timeout]. *)
val write : t -> string -> unit
