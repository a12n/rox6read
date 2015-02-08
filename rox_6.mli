module Bike_entry :
sig
  type t = {
      (* Data from sensors *)
      wheel_rot : int;
      speed : float;          (* km/h *)
      cadence : int;          (* rpm *)
      hr : int;               (* bpm *)
      alt : float;            (* m *)
      temp : int;             (* °C *)

      (* Derived fields *)
      distance : float;         (* m *)
      duration : int;         (* s *)

      (* Derived fields *)
      abs_distance : float;     (* m *)
      abs_duration : int;     (* s *)
      alt_diff : float;       (* m *)
      distance_uphill : float;  (* m *)
      duration_uphill : int;  (* s *)
      distance_downhill : float; (* m *)
      duration_downhill : int; (* s *)
    }
end

module Bike_lap :
sig
  type t = {
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
    }
end

module Bike_pause :
sig
  type t = string
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
         | Hike of Hike_entry.t
end

module Log_marker :
sig
  type t = Bike_lap of Bike_lap.t
         | Bike_pause of Bike_pause.t
         | Hike_pause of Hike_pause.t
end

module Log :
sig
  type t = {
      entry : Log_entry.t list;
      marker : Log_marker.t list;
    }
end

module Log_summary :
sig
  type t = {
      start_date : Date.t;
      start_time : Time.t;

      age : int;                          (* y *)
      mass : float;                       (* kg *)
      sex : Sex.t;

      max_hr : int;           (* bpm *)
      hr_limits : int * int;              (* bpm *)

      training_zone : Training_zone.t;
      zone_start : int * int * int * int; (* % *)

      bike_no : Bike_no.t;
      wheel_circum : float;     (* m *)

      distance : float;         (* m *)
      duration : int;           (* s *)
      max_speed : float;        (* km/h *)
      alt_gain : float;         (* m *)
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
      zone_start : int * int * int * int; (* % *)
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

module Totals :
sig
  type t = {
      (* Bike *)
      distance : float * float; (* m *)
      duration : int * int;     (* s *)
      alt_gain : float * float; (* m *)
      kcal : int * int;         (* kcal *)
      (* Hike *)
      hike_duration : int;      (* s *)
      hike_alt_gain : float;    (* m *)
      hike_kcal : int;          (* kcal *)
    }
end

exception Invalid_response of string

val log_summary : Ser_port.t -> Log_summary.t

val log : Ser_port.t -> Log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val settings : Ser_port.t -> Settings.t

val totals : Ser_port.t -> Totals.t
