open Batteries

let char_codes =
  Array.of_list % List.map Char.code % String.to_list

let valid_checksum c n =
  (Array.(sub c 0 n |> sum) land 0xFF) == c.(n)

let valid_padding c k =
  c.(k) == 0xA1 && c.(k + 1) == 0xA2 &&
    c.(k + 2) == 0xA3 && c.(k + 3) == 0xA4

module Bat_status =
  struct
    type t = Ok | Low

    let scan ans =
      let c = char_codes ans in
      if (Array.(sub c 0 6 |> sum) land 0x0F) != (c.(6) lsr 4) then
        failwith "Invalid battery status checksum";
      if (c.(2) land 0x80) == 0 then
        Ok
      else
        Low

    let recieve =
      scan % Dock.command ~code:0xEF ~address:0x6A00 ~ans_size:7
  end

module Settings =
  struct
    type t = {
        (* Person *)
        age : int;                (* y *)
        mass : int;               (* kg *)
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

    let scan ans =
      let c = char_codes ans in
      (* Checksums *)
      if not (valid_checksum c 29) then
        failwith "Invalid settings checksum";
      if not (valid_padding c 30) then
        failwith "Invalid settings padding";
      (* Scan binary data *)
      {
        (* Person *)
        age = c.(13);
        mass = c.(15) * 1000 + c.(14);
        sex = if (c.(28) land 0x10) == 0 then
                Sex.Male
              else
                Sex.Female ;
        (* Heart rate *)
        hr_max = c.(16);
        hr_limits = (c.(21), c.(22));
        (* Training zones *)
        training_zone =
          begin
            match (c.(9) land 0x60) lsr 5 with
              0 -> Training_zone.Fit
            | 1 -> Training_zone.Fat
            | 2 -> Training_zone.Own
            | _ -> failwith "Invalid training zone in settings"
          end ;
        zone_alarm = (c.(9) land 0x80) == 0;
        zone_start = (c.(17), c.(18), c.(19), c.(20));
        (* Bike *)
        wheel_size = (
          (* Bike1 *)
          ((c.(7) land 0x0F) lsl 8) lor c.(6)
        ,
          (* Bike2 *)
          ((c.(9) land 0x0F) lsl 8) lor c.(8)
        );
        (* Date and time *)
        date = {
            Date.y = ((c.(12) land 0x0F) lsl 8) lor c.(11);
            mon = ((c.(12) land 0xF0) lsr 4);
            d = c.(10) land 0x3F;
          };
        time = {
            Time.h = (c.(1) land 0x78) lsr 3;
            min = c.(2) land 0x3F;
            s = (c.(5) land 0xFC) lsr 2;
          };
        (* Altitude *)
        slp = (c.(0) lor ((c.(1) land 0x07) lsl 8)) * 10 + 90000;
        actual_alt =
          begin
            let alt = (((c.(24) land 0x7F) lsl 8) lor c.(23)) * 1000 +
                        (c.(25) land 0x0F) in
            if (c.(24) lsr 7) == 0 then
              alt
            else
              -alt
          end;
        home_alt =
          begin
            let alt = (((c.(27) land 0x7F) lsl 8) lor c.(26)) * 1000 +
                        (c.(28) land 0x0F) in
            if (c.(27) lsr 7) == 0 then
              alt
            else
              -alt
          end ;
        alt_ref = if (c.(25) land 0x80) == 0 then
                    Alt_ref.Actual_alt
                  else
                    Alt_ref.Slp;
        (* Device *)
        lang =
          begin
            match (c.(25) land 0x70) lsr 4 with
              0 -> Lang.De
            | 1 -> Lang.En
            | 2 -> Lang.Fr
            | 3 -> Lang.It
            | 4 -> Lang.Es
            | 5 -> Lang.Pl
            | 6 -> Lang.Nl
            | _ -> failwith "Invalid language in settings"
          end ;
        date_format =
          if (c.(28) land 0x80) == 0 then
            Date_format.Eu
          else
            Date_format.Us ;
        speed_unit =
          if (c.(2) land 0x40) == 0 then
            Speed_unit.Kmh
          else
            Speed_unit.Mph ;
        mass_unit =
          if (c.(2) land 0x40) == 0 then
            Mass_unit.Kg
          else
            Mass_unit.Lb ;
        contrast =
          begin
            match (c.(28) land 0x60) lsr 5 with
              1 -> Contrast.High
            | 2 -> Contrast.Mid
            | 3 -> Contrast.Low
            | _ -> failwith "Invalid contrast value in settings"
          end ;
        low_bat =
          begin
            match c.(7) lsr 4 with
              1 -> Low_bat.Below_1830
            | 2 -> Low_bat.Below_2000
            | 3 -> Low_bat.Below_2170
            | 4 -> Low_bat.Below_2330
            | 5 -> Low_bat.Below_2500
            | 6 -> Low_bat.Below_2670
            | 7 -> Low_bat.Below_2830
            | 8 -> Low_bat.Below_3000
            | _ -> failwith "Invalid low battery level in settings"
          end ;
        (* Service interval *)
        serv_interval = (
          (c.(9) land 0x10) != 0
        ,
          ((c.(5) land 0x03) lsl 16) lor (c.(4) lsl 8) lor c.(3)
        );
      }

    let recieve =
      scan % Dock.command ~code:0xEF ~address:0x2000 ~ans_size:34
  end

module Totals =
  struct
    type t = {
        distance : int * int;
        time : int * int;
        kcal : int * int;
        climb : int * int;
        hike_alt : int;
        hike_time : int;
        hike_kcal : int;
      }

    let scan ans =
      let c = char_codes ans in
      (* Checksums *)
      if not (valid_checksum c 35) then
        failwith "Invalid totals checksum";
      if not (valid_padding c 36) then
        failwith "Invalid totals padding";
      (* Scan binary data *)
      { distance = (
          (* Bike1 *)
          (c.( 0) lor (c.( 1) lsl 8) lor (c.( 2) lsl 16) lor ((c.( 3) land 0x0F) lsl 24)) +
            (((c.( 7) land 0xFC) lsl 2) lor ((c.( 3) land 0xF0) lsr 4)) / 1000
        ,
          (* Bike2 *)
          (c.( 8) lor (c.( 9) lsl 8) lor (c.(10) lsl 16) lor ((c.(11) land 0x0F) lsl 24)) +
            (((c.(15) land 0xFC) lsl 2) lor ((c.(11) land 0x0F) lsr 4)) / 1000
        );
        time = (
          (* Bike1 *)
          c.(4) lor (c.(5) lsl 8) lor (c.(6) lsl 16) lor (c.(7) land 0x03) lsl 24
        ,
          (* Bike2 *)
          ((c.(15) land 3) lsl 24) lor (c.(14) lsl 16) lor (c.(13) lsl 8) lor c.(12)
        );
        kcal = (
          (* Bike1 *)
          (((c.(25) land 0x04) lsr 2) lsl 16) lor (c.(17) lsl 8) lor c.(16)
        ,
          (* Bike2 *)
          (((c.(25) land 0x08) lsr 3) lsl 16) lor (c.(19) lsl 8) lor c.(18)
        );
        climb = (
          (* Bike1 *)
          (((c.(28) land 0x0F) lsl 16) lor (c.(27) lsl 8) lor c.(26)) * 100 +
            10 * ((c.(28) land 0xF0) lsr 4)
        ,
          (* Bike2 *)
          (((c.(31) land 0x0F) lsl 16) lor (c.(30) lsl 8) lor c.(29)) * 100 +
            10 * ((c.(31) land 0xF0) lsr 4)
        );
        hike_alt =
          (((c.(34) land 15) lsl 16) land (c.(33) lsl 8) lor c.(32)) * 100 +
            10 * ((c.(34) land 0xF0) lsr 4) ;
        hike_kcal =
          (((c.(25) land 0x10) lsr 4) lsl 16) lor (c.(21) lsl 8) lor c.(20) ;
        hike_time =
          ((c.(25) land 0x03) lsl 24) lor (c.(24) lsl 16) lor (c.(23) lsl 8) lor c.(22) }

    let recieve =
      scan % Dock.command ~code:0xEF ~address:0x4200 ~ans_size:40
  end
