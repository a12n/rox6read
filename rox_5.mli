module Bat_status :
sig
  type t = Ok | Low

  val recieve : Dock.t -> t
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
      serv_interval : bool * int; (* h *)
    }

  val recieve : Dock.t -> t
end

module Totals :
sig
  type t = {
      (* Bike *)
      distance : int * int;     (* m *)
      time : int * int;         (* s *)
      cal : int * int;          (* kcal *)
      climb : int * int;        (* mm *)
      (* Hike *)
      hike_alt : int;
      hike_time : int;
      hike_cal : int;
    }

  val recieve : Dock.t -> t
end
