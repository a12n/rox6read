(** Read/write to ROX {5,6}.0, Topline 2012 docking station *)

(** Abstract type with docking station state. *)
type t

(** {2 Open/close docking station} *)

(** Close connection to the docking station. *)
val close : t -> unit

(** Open connection to the docking station through serial device file
 * at [path]. *)
val open_dock : string -> t

(** {2 Connected Device} *)

(** Returns [true] if there's a cycling computer connected to the
 * docking station. *)
val device_connected : t -> bool

(** Information about cycling computer that is/was connected to the
 * docking station. *)
val device_info : t -> Device_info.t option

(** {2 I/O and Commands} *)

(** Send some [data] to the docking station, and receive [ans_size]
 * bytes of response. *)
val simple_command : t -> data:string -> ans_size:int -> string
