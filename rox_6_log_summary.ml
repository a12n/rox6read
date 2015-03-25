open Batteries

type t = {
    start_date : Date.t;
    start_time : Time.t;

    age : int;
    mass : float;
    sex : Sex.t;

    max_hr : int;
    hr_limits : int * int;

    training_zone : Training_zone.t;
    zone_start : float * float * float * float;

    bike_no : Bike_no.t;
    wheel_circum : float;

    distance : float;
    duration : int;
    max_speed : float * int;
    alt_gain : float;
    alt_loss : float;
    kcal : int;

    hike_duration : int;
    hike_alt_gain : float;
    hike_alt_loss : float;
    hike_kcal : int;

    speed_unit : Speed_unit.t;
    mass_unit : Mass_unit.t;

    log_size : int;
  }

let decode buf =
  let bytea = Bytea.of_bytes buf in
  (* Checksum *)
  Rox_6.verify_checksum bytea ~n:48;
  Rox_6.verify_padding bytea ~k:49;
  (* Parse binary data *)
  { max_hr =
      bytea.(0);
    zone_start =
      float_of_int bytea.(1) /. 100.0,
      float_of_int bytea.(2) /. 100.0,
      float_of_int bytea.(3) /. 100.0,
      float_of_int bytea.(4) /. 100.0;
    hr_limits =
      bytea.(5),
      bytea.(6);
    age =
      bytea.(7);
    mass =
      float_of_int (bytea.(9) * 1000 + bytea.(8)) /. 1000.0;
    log_size =
      ((bytea.(26) lsl 8) lor bytea.(25)) - Rox_6.log_addr;
    training_zone =
      (match (bytea.(30) land 0xC0) lsr 6 with
         0 -> Training_zone.Fit (* TODO: hr_limits = max_hr * 0.7, max_hr * 0.8 *)
       | 1 -> Training_zone.Fat (* TODO: hr_limits = max_hr * 0.55, max_hr * 0.7 *)
       | 2 -> Training_zone.Own
       | _ -> failwith "training_zone");
    sex =
      if (bytea.(31) land 0x40) == 0 then
        Sex.Male
      else
        Sex.Female;
    start_date =
      { Date.y = ((bytea.(36) land 0x0F) lsl 8) lor bytea.(35);
        mon = (bytea.(36) land 0xF0) lsr 4;
        d = bytea.(34) land 0x3F };
    start_time =
      { Time.h = bytea.(29) land 0x1F;
        min = bytea.(30) land 0x3F;
        s = bytea.(31) land 0x3F };
    (* Hike? *)
    mass_unit =
      if (bytea.(14) land 0x80) == 0 then
        Mass_unit.Kg
      else
        Mass_unit.Lb;
    hike_duration =
      ((bytea.(21) land 0x3F) lsl 16) lor
        (bytea.(20) lsl 8) lor
          bytea.(19);
    hike_kcal =
      ((bytea.(44) land 0x01) lsl 16) lor
        (bytea.(41) lsl 8) lor
          bytea.(40);
    hike_alt_gain =
      float_of_int (
          ((bytea.(44) land 0x0F) lsl 16) lor
            (bytea.(43) lsl 8) lor
              bytea.(42)
        ) /. 1000.0;
    hike_alt_loss =
      float_of_int (
          ((bytea.(47) land 0x0F) lsl 16) lor
            (bytea.(46) lsl 8) lor
              bytea.(45)
        ) /. 1000.0;
    (* Bike *)
    duration =
      ((bytea.(12) land 0x3F) lsl 16) lor
        (bytea.(11) lsl 8) lor
          bytea.(10);
    speed_unit =
      if (bytea.(14) land 0x80) == 0 then
        Speed_unit.Kmh
      else
        Speed_unit.Mph;
    max_speed =
      (let speed =
         float_of_int (((bytea.(14) land 0x7F) lsl 8) + bytea.(13)) /. 100.0 in
       let index =
         (bytea.(18) lsl 8) lor bytea.(17) in
       speed, index);
    alt_gain =
      float_of_int (
          ((bytea.(18) lsr 4) lsl 16) lor
            (bytea.(16) lsl 8) lor
              bytea.(15)
        ) /. 10.0;
    distance =
      float_of_int (
          (bytea.(24) lsl 16) lor
            (bytea.(23) lsl 8) lor
              bytea.(22)
        );
    kcal =
      ((bytea.(29) lsr 7) lsl 16) lor
        (bytea.(28) lsl 8) lor
          bytea.(27);
    bike_no =
      if (bytea.(31) land 0x80) == 0 then
        Bike_no.Bike_1
      else
        Bike_no.Bike_2;
    wheel_circum =
      float_of_int (
          ((bytea.(33) land 0x0F) lsl 8) lor
            bytea.(32)
        ) /. 1000.0;
    alt_loss =
      float_of_int (
          ((bytea.(39) land 0x0F) lsl 16) lor
            (bytea.(38) lsl 8) lor
              bytea.(37)
        ) /. 10.0;
  }

let recv = decode % Rox_6.run_command ~code:0xEF ~addr:0x0071 ~ans_size:53
