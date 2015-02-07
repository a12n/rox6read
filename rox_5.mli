module Log :
sig
  (* TODO *)
  type t = string
end

module Log_summary :
sig
  type t = {
      start_date : Date.t;
      start_time : Time.t;

      age : int;                          (* y *)
      mass : int;                         (* g *)
      sex : Sex.t;

      hr_max : int;           (* bpm *)
      hr_limits : int * int;              (* bpm *)

      training_zone : Training_zone.t;
      zone_start : int * int * int * int; (* % *)

      bike_no : Bike_no.t;
      wheel_size : int;         (* mm *)

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

      mass_unit : Mass_unit.t;
      speed_unit : Speed_unit.t;
      sample_interval : int;  (* s *)

      raw_size : int;        (* size of log in device memory *)

      max_speed_e : float;      (* ? *)
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
      hr_max : int;             (* bpm *)
      hr_limits : int * int;    (* bpm *)
      (* Training zones *)
      training_zone : Training_zone.t;
      zone_alarm : bool;
      zone_start : int * int * int * int; (* % *)
      (* Bike *)
      wheel_size : int * int;   (* mm *)
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

exception Invalid_checksum

val log_summary : Ser_port.t -> Log_summary.t

val log : Ser_port.t -> Log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool

val settings : Ser_port.t -> Settings.t

val totals : Ser_port.t -> Totals.t
