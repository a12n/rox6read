(** ROX {5,6}.0, Topline 2012 docking station protocol *)

(** Unsupported device is connected to the docking station *)
exception Unknown_device

(** Returns [true] if there's a cycling computer connected to the
 * docking station. *)
val device_connected : Ser_port.t -> bool

(** Information about cycling computer that is/was connected to the
 * docking station. *)
val device_info : Ser_port.t -> Device_info.t option
