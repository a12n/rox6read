open Batteries

let char_codes =
  Array.of_list % List.map Char.code % String.to_list

let valid_checksum c n =
  (Array.(sub c 0 n |> sum) land 0xFF) == c.(n)

let valid_padding c k =
  c.(k) == 0xA1 && c.(k + 1) == 0xA2 &&
    c.(k + 2) == 0xA3 && c.(k + 3) == 0xA4

let command_buf ~code ~address ~ans_size =
  let buf = IO.output_string () in
  IO.write_byte buf code;
  IO.write_ui16 buf address;
  IO.write_ui16 buf ans_size;
  IO.close_out buf

let fully_received ans =
  let n = String.length ans in
  n > 2 && ans.[n - 2] == '\x00' && ans.[n - 1] == '\xFF'

let command fd ~code ~address ~ans_size =
  Ser_port.write fd (command_buf ~code ~address ~ans_size);
  let ans = Ser_port.read fd (ans_size + 2) in
  if not (fully_received ans) then
    failwith "Missing end of response marker";
  String.sub ans 0 ans_size

let package_command fd ~code ~address ~ans_size =
  (* TODO: Use bytes *)
  let ans = String.make ans_size '\x00' in
  let max_ans_size = 768 in
  let rec aux off =
    if off < ans_size then
      begin
        let part_size = min (ans_size - off) max_ans_size in
        let part = command fd ~code ~address:(address + off) ~ans_size:part_size in
        String.blit part 0 ans off part_size;
        aux (off + part_size)
      end in
  aux 0;
  ans

exception Invalid_checksum

module Activity =
  struct
    (* TODO *)
    type t = string

    let address = 0x00A6

    let scan ans =
      ans
  end

module Activity_summary =
  struct
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

    let scan ans =
      let c = char_codes ans in
      (* Checksum *)
      if not (valid_checksum c 48) then
        raise Invalid_checksum;
      if not (valid_padding c 49) then
        raise Invalid_checksum;
      (* Parse binary data *)
      { sample_interval = 10;
        hr_max = c.(0);
        zone_start = c.(0) * c.(1) / 100,
                     c.(0) * c.(2) / 100,
                     c.(0) * c.(3) / 100,
                     c.(0) * c.(4) / 100;
        hr_limits = c.(5), c.(6);
        age = c.(7);
        mass = c.(9) * 1000 + c.(8);
        raw_size = ((c.(26) lsl 8) lor c.(25)) - Activity.address;
        training_zone =
          begin
            match (c.(30) land 0xC0) lsr 6 with
              0 -> Training_zone.Fit (* TODO: hr_limits = hr_max * 0.7, hr_max * 0.8 *)
            | 1 -> Training_zone.Fat (* TODO: hr_limits = hr_max * 0.55, hr_max * 0.7 *)
            | 2 -> Training_zone.Own
            | _ -> failwith "Invalid training zone in activity summary"
          end ;
        sex = if (c.(31) land 0x40) == 0 then
                Sex.Male
              else
                Sex.Female ;
        start_date = { Date.y = ((c.(36) land 0x0F) lsl 8) lor c.(35);
                       mon = (c.(36) land 0xF0) lsr 4;
                       d = c.(34) land 0x3F };
        start_time = { Time.h = c.(29) land 0x1F;
                       min = c.(30) land 0x3F;
                       s = c.(31) land 0x3F } }
  end

module Bat_low =
  struct
    let scan ans =
      let c = char_codes ans in
      if (Array.(sub c 0 6 |> sum) land 0x0F) != (c.(6) lsr 4) then
        raise Invalid_checksum;
      (c.(2) land 0x80) != 0
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
        raise Invalid_checksum;
      if not (valid_padding c 30) then
        raise Invalid_checksum;
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
  end

module Totals =
  struct
    type t = {
        distance : int * int;
        duration : int * int;
        kcal : int * int;
        climb : int * int;
        hike_alt : int;
        hike_duration : int;
        hike_kcal : int;
      }

    let scan ans =
      let c = char_codes ans in
      (* Checksums *)
      if not (valid_checksum c 35) then
        raise Invalid_checksum;
      if not (valid_padding c 36) then
        raise Invalid_checksum;
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
        duration = (
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
        hike_duration =
          ((c.(25) land 0x03) lsl 24) lor (c.(24) lsl 16) lor (c.(23) lsl 8) lor c.(22) }
  end

let activity_summary =
  Activity_summary.scan % command ~code:0xEF ~address:0x0071 ~ans_size:53

let activity port { Activity_summary.raw_size; _ } =
  package_command port ~code:0xEF ~address:Activity.address ~ans_size:raw_size |> Activity.scan

let bat_low =
  Bat_low.scan % command ~code:0xEF ~address:0x006A ~ans_size:7

let settings =
  Settings.scan % command ~code:0xEF ~address:0x0020 ~ans_size:34

let totals =
  Totals.scan % command ~code:0xEF ~address:0x0042 ~ans_size:40
