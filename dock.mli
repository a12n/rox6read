(** Read/write to ROX {5,6}.0, Topline 2012 docking station *)

(** Abstract type with docking station state. *)
type t

(** Close connection to the docking station. *)
val close : t -> unit

(** Send some [data] to the docking station, and receive [n] bytes of
 * response. *)
val command : t -> data:string -> ans_size:int -> string

(** Returns [true] if there's a cycling computer connected to the
 * docking station. *)
val device_connected : t -> bool

(** Model of cycling computer device that is/was connected to the
 * docking station. *)
val device_model : t -> Device_model.t option

(** Open connection to the docking station through serial device file
 * at [path]. *)
val open_dock : string -> t
