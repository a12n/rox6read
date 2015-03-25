module Bike_entry :
sig
  type t = {
      ts : int;                 (* s *)
      wheel_rot : int;
      duration : int;           (* s *)
      speed : float;          (* km/h *)
      cadence : int;          (* rpm *)
      hr : int;               (* bpm *)
      alt : float;            (* m *)
      temp : int;             (* °C *)

      distance : float;         (* m *)
      abs_distance : float;     (* m *)
      unadj_distance : float;   (* m *)
    }
end

module Bike_lap :
sig
  type t = {
      ts : int;                 (* s *)
      wheel_rot : int;
      duration : int;         (* s *)
      avg_speed : float;      (* km/h *)
      avg_hr : int;           (* bpm *)
      max_hr : int;           (* bpm *)
      avg_cadence : int;      (* rpm *)
      kcal : int;             (* kcal *)
      max_speed : float;      (* km/h *)
      alt_gain : float;       (* m *)
      alt_loss : float;       (* m *)

      distance : float;         (* m *)
      abs_distance : float;     (* m *)
    }
end

module Bike_pause :
sig
  type t = {
      ts : int;                 (* s *)
      wheel_rot : int;
      avg_alt : float;          (* m *)
      start_date : Date.t;
      start_time : Time.t;
      stop_date : Date.t;
      stop_time : Time.t;

      distance : float;         (* m *)
      abs_distance : float;     (* m *)
    }
end

module Hike_entry :
sig
  type t = string
end

module Hike_pause :
sig
  type t = string
end

module Log_entry :
sig
  type t = Bike of Bike_entry.t
         | Bike_lap of Bike_lap.t
         | Bike_pause of Bike_pause.t
         | Hike of Hike_entry.t
         | Hike_pause of Hike_pause.t
end

module Log :
sig
  type t = Log_entry.t list
end

exception Invalid_response of string

val log : Ser_port.t -> Rox_6_log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val log_addr : int

val verify_checksum : Bytea.t -> n:int -> unit

val verify_padding : Bytea.t -> k:int -> unit

val run_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes

val run_pkg_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes
