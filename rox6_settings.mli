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
