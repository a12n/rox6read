(** Read/write to ROX {5,6}.0, Topline 2012 docking station *)

type t

val close : t -> unit
val open_dock : string -> t

val unit_connected : t -> bool
val unit_model : t -> Unit_model.t

val read : t -> int -> string
val write : t -> string -> unit
