open Batteries

exception Invalid_response of string

let log_address = 0x00A6

let sample_interval = 10

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
    raise (Invalid_response "end of response marker");
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

module Log_summary =
  struct
    type t = {
        start_date : Date.t;
        start_time : Time.t;

        age : int;              (* y *)
        mass : float;           (* kg *)
        sex : Sex.t;

        max_hr : int;           (* bpm *)
        hr_limits : int * int;  (* bpm *)

        training_zone : Training_zone.t;
        zone_start : float * float * float * float; (* frac of max_hr *)

        bike_no : Bike_no.t;
        wheel_circum : float;   (* m *)

        distance : float;       (* m *)
        duration : int;         (* s *)
        max_speed : float * int; (* km/h, log entry index *)
        alt_gain : float;       (* m *)
        alt_loss : float;       (* m *)
        kcal : int;             (* kcal *)

        hike_duration : int;    (* s *)
        hike_alt_gain : float;  (* m *)
        hike_alt_loss : float;  (* m *)
        hike_kcal : int;        (* kcal *)

        speed_unit : Speed_unit.t;
        mass_unit : Mass_unit.t;

        log_size : int;        (* size of log in device memory *)
      }

    let scan ans =
      let c = char_codes ans in
      (* Checksum *)
      if not (valid_checksum c 48) then
        raise (Invalid_response "checksum");
      if not (valid_padding c 49) then
        raise (Invalid_response "padding");
      (* Parse binary data *)
      { max_hr = c.(0);
        zone_start = float_of_int c.(1) /. 100.0,
                     float_of_int c.(2) /. 100.0,
                     float_of_int c.(3) /. 100.0,
                     float_of_int c.(4) /. 100.0;
        hr_limits = c.(5), c.(6);
        age = c.(7);
        mass = float_of_int (c.(9) * 1000 + c.(8)) /. 1000.0;
        log_size = ((c.(26) lsl 8) lor c.(25)) - log_address;
        training_zone =
          begin
            match (c.(30) land 0xC0) lsr 6 with
              0 -> Training_zone.Fit (* TODO: hr_limits = max_hr * 0.7, max_hr * 0.8 *)
            | 1 -> Training_zone.Fat (* TODO: hr_limits = max_hr * 0.55, max_hr * 0.7 *)
            | 2 -> Training_zone.Own
            | _ -> raise (Invalid_response "training zone")
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
                       s = c.(31) land 0x3F };
        (* Hike? *)
        mass_unit = if (c.(14) land 0x80) == 0 then
                      Mass_unit.Kg
                    else
                      Mass_unit.Lb ;
        hike_duration = ((c.(21) land 0x3F) lsl 16) lor (c.(20) lsl 8) lor c.(19) ;
        hike_kcal = ((c.(44) land 0x01) lsl 16) lor (c.(41) lsl 8) lor c.(40) ;
        hike_alt_gain = float_of_int (
                            ((c.(44) land 0x0F) lsl 16) lor (c.(43) lsl 8) lor c.(42) (* mm *)
                          ) /. 1000.0;
        hike_alt_loss = float_of_int (
                            ((c.(47) land 0x0F) lsl 16) lor (c.(46) lsl 8) lor c.(45) (* mm *)
                          ) /. 1000.0;
        (* Bike *)
        duration = ((c.(12) land 0x3F) lsl 16) lor (c.(11) lsl 8) lor c.(10) ;
        speed_unit = if (c.(14) land 0x80) == 0 then
                       Speed_unit.Kmh
                     else
                       Speed_unit.Mph ;
        max_speed = (
          float_of_int (((c.(14) land 0x7F) lsl 8) + c.(13)) /. 100.0
        ,
          (c.(18) lsl 8) lor c.(17)
        );
        alt_gain = float_of_int (
                       ((c.(18) lsr 4) lsl 16) lor (c.(16) lsl 8) lor c.(15) (* dm *)
                     ) /. 10.0;
        distance = float_of_int (
                       (c.(24) lsl 16) lor (c.(23) lsl 8) lor c.(22)
                     );
        kcal = ((c.(29) lsr 7) lsl 16) lor (c.(28) lsl 8) lor c.(27) ;
        bike_no = if (c.(31) land 0x80) == 0 then
                    Bike_no.Bike_1
                  else
                    Bike_no.Bike_2 ;
        wheel_circum = float_of_int (
                           ((c.(33) land 0x0F) lsl 8) lor c.(32) (* mm *)
                         ) /. 1000.0;
        alt_loss = float_of_int (
                       ((c.(39) land 0x0F) lsl 16) lor (c.(38) lsl 8) lor c.(37) (* dm *)
                     ) /. 10.0;
      }
  end

module Bike_entry =
  struct
    type t = {
        wheel_rot : int;
        speed : float;          (* km/h *)
        cadence : int;          (* rpm *)
        hr : int;               (* bpm *)
        alt : float;            (* m *)
        temp : int;             (* °C *)

        distance : float;       (* m *)
        duration : int;         (* s *)

        abs_distance : float;    (* m *)
        abs_duration : int;     (* s *)
        alt_diff : float;       (* m *)
        distance_uphill : float; (* m *)
        duration_uphill : int;  (* s *)
        distance_downhill : float; (* m *)
        duration_downhill : int; (* s *)
      }

    let size = 9

    let scan { Log_summary.wheel_circum; _ } buf =
      let c = char_codes buf in
      { wheel_rot = ((c.(2) land 0x03) lsl 8) lor c.(1);
        speed = float_of_int (((c.(4) land 0x7F) lsl 8) lor c.(3)) /. 100.0;
        cadence = c.(6);
        hr = c.(5);
        alt =
          begin
            let alt = float_of_int (((c.(8) land 0x7F) lsl 8) lor c.(7)) in
            if (c.(8) lsr 7) == 0 then
              alt
            else
              -.alt
          end;
        temp = (c.(2) lsr 2) - 10;
        (* Derived fields *)
        distance = 0.0;
        duration = sample_interval;
        (* Derived fields *)
        abs_distance = 0.0;
        abs_duration = 0;
        alt_diff = 0.0;
        distance_uphill = 0.0;
        duration_uphill = 0;
        distance_downhill = 0.0;
        duration_downhill = 0;
      }
  end

module Bike_lap =
  struct
    type t = {
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
      }

    let size = 23

    let scan buf =
      let c = char_codes buf in
      { wheel_rot = (c.(8) lsl 16) lor (c.(7) lsl 8) lor c.(6);
        duration = ((c.(3) land 0x3F) lsl 16) lor (c.(2) lsl 8) lor c.(1);
        avg_speed = float_of_int (((c.(5) land 0x7F) lsl 8) lor c.(4)) /. 100.0;
        avg_hr = c.(9);
        max_hr = c.(10);
        avg_cadence = c.(11);
        kcal = ((c.(15) lsr 7) lsl 16) lor (c.(13) lsl 8) lor (c.(12));
        max_speed = float_of_int (((c.(15) land 0x7F) lsl 8) lor c.(14)) /. 100.0;
        alt_gain = float_of_int (
                       (c.(18) lsl 16) lor (c.(17) lsl 8) lor c.(16) (* dm *)
                     ) /. 10.0;
        alt_loss = float_of_int (
                       (c.(21) lsl 16) lor (c.(20) lsl 8) lor c.(19) (* dm *)
                     ) /. 10.0;
      }
  end

module Bike_pause =
  struct
    type t = string

    let size = 21

    let scan buf = buf
  end

module Hike_entry =
  struct
    type t = string

    let size = 5

    let scan buf = buf
  end

module Hike_pause =
  struct
    type t = string

    let size = 16

    let scan buf = buf
  end

module Log_entry =
  struct
    type t = Bike of Bike_entry.t
           | Hike of Hike_entry.t
  end

module Log_marker =
  struct
    type t = Bike_lap of Bike_lap.t
           | Bike_pause of Bike_pause.t
           | Hike_pause of Hike_pause.t
  end

module Log =
  struct
    type t = {
        entry : Log_entry.t list;
        marker : Log_marker.t list;
      }

    let scan summary buf =
      let n = String.length buf in
      let rec aux k ans =
        if k < n then
          begin
            match (Char.code buf.[k]) land 0x07 with
            | 0 ->
               let e = String.sub buf k Bike_entry.size |> Bike_entry.scan summary in
               aux (k + Bike_entry.size) { ans with entry = (Log_entry.Bike e) :: ans.entry }
            | 1 ->
               let m = String.sub buf k Bike_pause.size |> Bike_pause.scan in
               aux (k + Bike_pause.size) { ans with marker = (Log_marker.Bike_pause m) :: ans.marker }
            | 2 ->
               let m = String.sub buf k Bike_lap.size |> Bike_lap.scan in
               aux (k + Bike_lap.size) { ans with marker = (Log_marker.Bike_lap m) :: ans.marker }
            | 3 ->
               let e = String.sub buf k Hike_entry.size |> Hike_entry.scan in
               aux (k + Hike_entry.size) { ans with entry = (Log_entry.Hike e) :: ans.entry }
            | 4 ->
               let m = String.sub buf k Hike_pause.size |> Hike_pause.scan in
               aux (k + Hike_pause.size) { ans with marker = (Log_marker.Hike_pause m) :: ans.marker }
            | _ -> raise (Invalid_response "log entry type")
          end
        else
          { ans with entry = List.rev ans.entry;
                     marker = List.rev ans.marker } in
      aux 0 { entry = []; marker = [] }
  end

module Bat_low =
  struct
    let scan ans =
      let c = char_codes ans in
      if (Array.(sub c 0 6 |> sum) land 0x0F) != (c.(6) lsr 4) then
        raise (Invalid_response "checksum");
      (c.(2) land 0x80) != 0
  end

module Settings =
  struct
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
        serv_interval : bool * int; (* h *)
      }

    let scan ans =
      let c = char_codes ans in
      (* Checksums *)
      if not (valid_checksum c 29) then
        raise (Invalid_response "checksum");
      if not (valid_padding c 30) then
        raise (Invalid_response "padding");
      (* Scan binary data *)
      {
        (* Person *)
        age = c.(13);
        mass = float_of_int (c.(15) * 1000 + c.(14)) /. 1000.0;
        sex = if (c.(28) land 0x10) == 0 then
                Sex.Male
              else
                Sex.Female ;
        (* Heart rate *)
        max_hr = c.(16);
        hr_limits = (c.(21), c.(22));
        (* Training zones *)
        training_zone =
          begin
            match (c.(9) land 0x60) lsr 5 with
              0 -> Training_zone.Fit
            | 1 -> Training_zone.Fat
            | 2 -> Training_zone.Own
            | _ -> raise (Invalid_response "training zone")
          end ;
        zone_alarm = (c.(9) land 0x80) == 0;
        zone_start = float_of_int c.(17) /. 100.0,
                     float_of_int c.(18) /. 100.0,
                     float_of_int c.(19) /. 100.0,
                     float_of_int c.(20) /. 100.0;
        (* Bike *)
        wheel_circum = (
          (* Bike1 *)
          float_of_int (
              ((c.(7) land 0x0F) lsl 8) lor c.(6) (* mm *)
            ) /. 1000.0
        ,
          (* Bike2 *)
          float_of_int (
              ((c.(9) land 0x0F) lsl 8) lor c.(8) (* mm *)
            ) /. 1000.0
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
            let alt = float_of_int (
                          (((c.(24) land 0x7F) lsl 8) lor c.(23)) * 1000 +
                            (c.(25) land 0x0F)
                        ) in
            ( if (c.(24) lsr 7) == 0 then
                alt
              else
                -.alt ) /. 1000.0
          end;
        home_alt =
          begin
            let alt = float_of_int (
                          (((c.(27) land 0x7F) lsl 8) lor c.(26)) * 1000 +
                            (c.(28) land 0x0F)
                        ) in
            ( if (c.(27) lsr 7) == 0 then
                alt
              else
                -.alt ) /. 1000.0
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
            | _ -> raise (Invalid_response "language")
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
            | _ -> raise (Invalid_response "contrast value")
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
            | _ -> raise (Invalid_response "low battery level")
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
        distance : float * float;
        duration : int * int;
        alt_gain : float * float;
        kcal : int * int;
        hike_duration : int;
        hike_alt_gain : float;
        hike_kcal : int;
      }

    let scan ans =
      let c = char_codes ans in
      (* Checksums *)
      if not (valid_checksum c 35) then
        raise (Invalid_response "checksum");
      if not (valid_padding c 36) then
        raise (Invalid_response "padding");
      (* Scan binary data *)
      { distance = (
          (* Bike1 *)
          float_of_int (
              (c.( 0) lor (c.( 1) lsl 8) lor (c.( 2) lsl 16) lor ((c.( 3) land 0x0F) lsl 24)) +
                (((c.( 7) land 0xFC) lsl 2) lor ((c.( 3) land 0xF0) lsr 4)) / 1000
            )
        ,
          (* Bike2 *)
          float_of_int (
              (c.( 8) lor (c.( 9) lsl 8) lor (c.(10) lsl 16) lor ((c.(11) land 0x0F) lsl 24)) +
                (((c.(15) land 0xFC) lsl 2) lor ((c.(11) land 0x0F) lsr 4)) / 1000
            )
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
        alt_gain = (
          (* Bike1 *)
          float_of_int (
              (((c.(28) land 0x0F) lsl 16) lor (c.(27) lsl 8) lor c.(26)) * 100 +
                10 * ((c.(28) land 0xF0) lsr 4)
            ) /. 1000.0
        ,
          (* Bike2 *)
          float_of_int (
              (((c.(31) land 0x0F) lsl 16) lor (c.(30) lsl 8) lor c.(29)) * 100 +
                10 * ((c.(31) land 0xF0) lsr 4)
            ) /. 1000.0
        );
        hike_alt_gain =
          float_of_int (
              (((c.(34) land 15) lsl 16) land (c.(33) lsl 8) lor c.(32)) * 100 +
                10 * ((c.(34) land 0xF0) lsr 4)
            ) /. 1000.0;
        hike_kcal =
          (((c.(25) land 0x10) lsr 4) lsl 16) lor (c.(21) lsl 8) lor c.(20) ;
        hike_duration =
          ((c.(25) land 0x03) lsl 24) lor (c.(24) lsl 16) lor (c.(23) lsl 8) lor c.(22) }
  end

let log_summary =
  Log_summary.scan % command ~code:0xEF ~address:0x0071 ~ans_size:53

let log port ({ Log_summary.log_size; _ } as summary) =
  package_command port ~code:0xEF ~address:log_address ~ans_size:log_size |> Log.scan summary

let bat_low =
  Bat_low.scan % command ~code:0xEF ~address:0x006A ~ans_size:7

let settings =
  Settings.scan % command ~code:0xEF ~address:0x0020 ~ans_size:34

let totals =
  Totals.scan % command ~code:0xEF ~address:0x0042 ~ans_size:40
