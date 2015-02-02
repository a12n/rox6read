(** Read/write to ROX {5,6}.0, Topline 2012 docking station *)

(** Abstract type with docking station state. *)
type t

(** Close connection to the docking station. *)
val close : t -> unit

(** Open connection to the docking station through serial device file
 * at [path]. *)
val open_dock : string -> t

(** Read [n] bytes of data from the docking station. *)
val read : t -> int -> string

(** Returns [true] if there's a cycling computer unit connected to the
 * docking station. *)
val unit_connected : t -> bool

(** Model of cycling computer unit that is/was connected to the
 * docking station. *)
val unit_model : t -> Unit_model.t option

(** Write [data] to the docking station. *)
val write : t -> string -> unit
