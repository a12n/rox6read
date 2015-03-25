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

  val recv : Ser_port.t -> t
end

module Totals :
sig
  type t = {
      distance : float * float;
      duration : int * int;
      alt_gain : float * float;
      kcal : int * int;
      hike_duration : int;
      hike_alt_gain : float;
      hike_kcal : int;
    }

  val recv : Ser_port.t -> t
end

exception Invalid_response of string

val log : Ser_port.t -> Log_summary.t -> Log.t

val bat_low : Ser_port.t -> bool
