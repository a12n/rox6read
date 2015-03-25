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

module Settings :
sig
  type t = {
      (* Person *)
      age : int;                (* y *)
      mass : float;             (* kg *)
      sex : Sex.t;
      (* Heart rate *)
      max_hr : int;             (* bpm *)
      hr_limits : int * int;    (* bpm *)
      (* Training zones *)
      training_zone : Training_zone.t;
      zone_alarm : bool;
      zone_start : float * float * float * float; (* frac of max_hr *)
      (* Bike *)
      wheel_circum : float * float; (* m *)
      (* Date and time *)
      date : Date.t;
      time : Time.t;
      (* Altitude *)
      slp : int;                (* Pa *)
      actual_alt : float;       (* m *)
      home_alt : float;         (* m *)
      alt_ref : Alt_ref.t;
      (* Device *)
      lang : Lang.t;
      date_format : Date_format.t;
      speed_unit : Speed_unit.t;
      mass_unit : Mass_unit.t;
      contrast : Contrast.t;
      low_bat : Low_bat.t;
      (* Service interval *)
      serv_interval : bool * int; (* km *)
    }
end

exception Invalid_response of string

val log : Ser_port.t -> Rox_6_log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val settings : Ser_port.t -> Settings.t

val log_addr : int

val verify_checksum : Bytea.t -> n:int -> unit

val verify_padding : Bytea.t -> k:int -> unit

val run_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes

val run_pkg_command : Ser_port.t -> code:int -> addr:int -> ans_size:int -> bytes
