open Batteries

type t = {
    age : int;
    mass : float;
    sex : Sex.t;

    max_hr : int;
    hr_limits : int * int;

    training_zone : Training_zone.t;
    zone_alarm : bool;
    zone_start : float * float * float * float;

    wheel_circum : float * float;

    date : Date.t;
    time : Time.t;

    slp : int;
    actual_alt : float;
    home_alt : float;
    alt_ref : Alt_ref.t;

    lang : Lang.t;
    date_format : Date_format.t;
    speed_unit : Speed_unit.t;
    mass_unit : Mass_unit.t;
    contrast : Contrast.t;
    low_bat : Low_bat.t;

    serv_interval : bool * int;
  }

let decode buf =
  let bytea = Bytea.of_bytes buf in
  (* Checksums *)
  Rox6.verify_checksum bytea ~n:29;
  Rox6.verify_padding bytea ~k:30;
  (* Scan binary data *)
  { (* Person *)
    age =
      bytea.(13);
    mass =
      float_of_int (bytea.(15) * 1000 + bytea.(14)) /. 1000.0;
    sex =
      if (bytea.(28) land 0x10) == 0 then
        Sex.Male
      else
        Sex.Female;
    (* Heart rate *)
    max_hr =
      bytea.(16);
    hr_limits =
      (let lower = bytea.(21) in
       let upper = bytea.(22) in
       lower, upper);
    (* Training zones *)
    training_zone =
      (match (bytea.(9) land 0x60) lsr 5 with
         0 -> Training_zone.Fit
       | 1 -> Training_zone.Fat
       | 2 -> Training_zone.Own
       | _ -> failwith "training_zone");
    zone_alarm =
      (bytea.(9) land 0x80) == 0;
    zone_start =
      float_of_int bytea.(17) /. 100.0,
      float_of_int bytea.(18) /. 100.0,
      float_of_int bytea.(19) /. 100.0,
      float_of_int bytea.(20) /. 100.0;
    (* Bike *)
    wheel_circum =
      (let bike_1 =
         float_of_int (
             ((bytea.(7) land 0x0F) lsl 8) lor
               bytea.(6)
           ) /. 1000.0 in
       let bike_2 =
         float_of_int (
             ((bytea.(9) land 0x0F) lsl 8) lor
               bytea.(8)
           ) /. 1000.0 in
       bike_1, bike_2);
    (* Date and time *)
    date =
      { Date.y = ((bytea.(12) land 0x0F) lsl 8) lor
                   bytea.(11);
        mon = ((bytea.(12) land 0xF0) lsr 4);
        d = bytea.(10) land 0x3F };
    time =
      { Time.h = (bytea.(1) land 0x78) lsr 3;
        min = bytea.(2) land 0x3F;
        s = (bytea.(5) land 0xFC) lsr 2 };
    (* Altitude *)
    slp =
      (bytea.(0) lor ((bytea.(1) land 0x07) lsl 8)) * 10 + 90000;
    actual_alt =
      (let abs_alt =
         float_of_int (
             (((bytea.(24) land 0x7F) lsl 8) lor bytea.(23)) * 1000 +
               (bytea.(25) land 0x0F)
           ) /. 1000.0 in
       if (bytea.(24) lsr 7) == 0 then
         abs_alt
       else
         -.abs_alt);
    home_alt =
      (let abs_alt =
         float_of_int (
             (((bytea.(27) land 0x7F) lsl 8) lor bytea.(26)) * 1000 +
               (bytea.(28) land 0x0F)
           ) /. 1000.0 in
       if (bytea.(27) lsr 7) == 0 then
         abs_alt
       else
         -.abs_alt);
    alt_ref =
      if (bytea.(25) land 0x80) == 0 then
        Alt_ref.Actual_alt
      else
        Alt_ref.Slp;
    (* Device *)
    lang =
      (match (bytea.(25) land 0x70) lsr 4 with
         0 -> Lang.De
       | 1 -> Lang.En
       | 2 -> Lang.Fr
       | 3 -> Lang.It
       | 4 -> Lang.Es
       | 5 -> Lang.Pl
       | 6 -> Lang.Nl
       | _ -> failwith "lang");
    date_format =
      if (bytea.(28) land 0x80) == 0 then
        Date_format.Eu
      else
        Date_format.Us;
    speed_unit =
      if (bytea.(2) land 0x40) == 0 then
        Speed_unit.Kmh
      else
        Speed_unit.Mph;
    mass_unit =
      if (bytea.(2) land 0x40) == 0 then
        Mass_unit.Kg
      else
        Mass_unit.Lb;
    contrast =
      (match (bytea.(28) land 0x60) lsr 5 with
         1 -> Contrast.High
       | 2 -> Contrast.Mid
       | 3 -> Contrast.Low
       | _ -> failwith "contrast");
    low_bat =
      (match bytea.(7) lsr 4 with
         1 -> Low_bat.Below_1830
       | 2 -> Low_bat.Below_2000
       | 3 -> Low_bat.Below_2170
       | 4 -> Low_bat.Below_2330
       | 5 -> Low_bat.Below_2500
       | 6 -> Low_bat.Below_2670
       | 7 -> Low_bat.Below_2830
       | 8 -> Low_bat.Below_3000
       | _ -> failwith "low_bat");
    (* Service interval *)
    serv_interval =
      (let enabled = (bytea.(9) land 0x10) != 0 in
       let after_km = ((bytea.(5) land 0x03) lsl 16) lor
                        (bytea.(4) lsl 8) lor
                          bytea.(3) in
       enabled, after_km);
  }

let recv = decode % Rox6.run_command ~code:0xEF ~addr:0x0020 ~ans_size:34
