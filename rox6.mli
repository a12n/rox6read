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

module Log_summary :
sig
  type t = {
      start_date : Date.t;        (**  *)
      start_time : Time.t;

      (** Rider's age (years). *)
      age : int;
      (** Rider's body mass (kilograms). *)
      mass : float;
      (** Rider's sex. *)
      sex : Sex.t;

      (** Rider's maximum heart rate (beats per minute). *)
      max_hr : int;
      (** Lower and upper heart rate limits of rider's own training zone
       * (beats per minute). *)
      hr_limits : int * int;

      training_zone : Training_zone.t;
      zone_start : float * float * float * float; (* frac of max_hr *)

      bike_no : Bike_no.t;
      wheel_circum : float;     (* m *)

      distance : float;         (* m *)
      duration : int;           (* s *)
      max_speed : float * int;  (* km/h, log entry index *)
      alt_gain : float;         (* m*)
      alt_loss : float;         (* m *)
      kcal : int;               (* kcal *)

      hike_duration : int;      (* s *)
      hike_alt_gain : float;    (* m *)
      hike_alt_loss : float;    (* m *)
      hike_kcal : int;          (* kcal *)

      speed_unit : Speed_unit.t;
      mass_unit : Mass_unit.t;

      log_size : int;        (* size of log in device memory *)
    }

  val recv : Ser_port.t -> t
end

module Settings : (module type of Rox6_settings)
module Totals : (module type of Rox6_totals)

exception Invalid_response of string

val log : Ser_port.t -> Log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val log_addr : int

val verify_checksum : Bytea.t -> n:int -> unit

val verify_padding : Bytea.t -> k:int -> unit

val run_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes

val run_pkg_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes
