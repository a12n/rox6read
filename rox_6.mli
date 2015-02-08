module Bike_entry :
sig
  type t = {
      (* Data from sensors *)
      wheel_rot : int;
      temp : int;             (* °C *)
      speed : float;          (* km/h *)
      hr : int;               (* bpm *)
      cadence : int;          (* rpm *)
      alt : int;              (* mm *)

      (* Derived fields *)
      distance : int;         (* m *)
      duration : int;         (* s *)
      abs_distance : int;     (* m *)
      abs_duration : int;     (* s *)

      (* Derived fields *)
      alt_diff : int;         (* mm *)
      distance_uphill : int;  (* m *)
      duration_uphill : int;  (* s *)
      distance_downhill : int; (* m *)
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
      alt_gain : int;         (* mm *)
      alt_loss : int;         (* mm *)
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
      mass : int;                         (* g *)
      sex : Sex.t;

      max_hr : int;           (* bpm *)
      hr_limits : int * int;              (* bpm *)

      training_zone : Training_zone.t;
      zone_start : int * int * int * int; (* % *)

      bike_no : Bike_no.t;
      wheel_circum : int;       (* mm *)

      distance : int;           (* m *)
      duration : int;           (* s *)
      max_speed : float;        (* km/h *)
      alt_gain : int;           (* mm *)
      alt_loss : int;           (* mm *)
      kcal : int;               (* kcal *)

      hike_duration : int;      (* s *)
      hike_alt_gain : int;      (* mm *)
      hike_alt_loss : int;      (* mm *)
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
      mass : int;               (* g *)
      sex : Sex.t;
      (* Heart rate *)
      max_hr : int;             (* bpm *)
      hr_limits : int * int;    (* bpm *)
      (* Training zones *)
      training_zone : Training_zone.t;
      zone_alarm : bool;
      zone_start : int * int * int * int; (* % *)
      (* Bike *)
      wheel_circum : int * int; (* mm *)
      (* Date and time *)
      date : Date.t;
      time : Time.t;
      (* Altitude *)
      slp : int;                (* Pa *)
      actual_alt : int;         (* mm *)
      home_alt : int;           (* mm *)
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
      distance : int * int;     (* m *)
      duration : int * int;     (* s *)
      alt_gain : int * int;     (* mm *)
      kcal : int * int;         (* kcal *)
      (* Hike *)
      hike_duration : int;      (* s *)
      hike_alt_gain : int;      (* mm *)
      hike_kcal : int;          (* kcal *)
    }
end

exception Invalid_response of string

val log_summary : Ser_port.t -> Log_summary.t

val log : Ser_port.t -> Log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val settings : Ser_port.t -> Settings.t

val totals : Ser_port.t -> Totals.t
