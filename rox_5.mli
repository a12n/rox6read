module Activity :
sig
  (* TODO *)
  type t = string
end

module Activity_summary :
sig
  type t = {
      sample_interval : int;  (* s *)
      hr_max : int;           (* bpm *)
      zone_start : int * int * int * int; (* % *)
      hr_limits : int * int;              (* bpm *)
      age : int;                          (* y *)
      mass : int;                         (* g *)
      raw_size : int;        (* size of activity in device memory *)
      training_zone : Training_zone.t;
      sex : Sex.t;
      start_date : Date.t;
      start_time : Time.t;
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
      time : int * int;         (* s *)
      kcal : int * int;         (* kcal *)
      climb : int * int;        (* mm *)
      (* Hike *)
      hike_alt : int;           (* mm *)
      hike_time : int;          (* s *)
      hike_kcal : int;          (* kcal *)
    }
end

exception Invalid_checksum

val activity_summary : Ser_port.t -> Activity_summary.t

val activity : Ser_port.t -> Activity_summary.t -> Activity.t

val bat_low : Ser_port.t -> bool

val settings : Ser_port.t -> Settings.t

val totals : Ser_port.t -> Totals.t
