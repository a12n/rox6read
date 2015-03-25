open Batteries

exception Invalid_response of string

let log_addr = 0x00A6

let sample_interval = 10

let char_codes =
  Array.of_list % List.map Char.code % String.to_list

let verify_checksum bytea ~n =
  if bytea.(n) <> Array.(sub bytea 0 n |> sum) land 0xFF then
    failwith "verify_checksum"

let verify_padding bytea ~k =
  if bytea.(k) <> 0xA1 || bytea.(k + 1) <> 0xA2 ||
       bytea.(k + 2) <> 0xA3 || bytea.(k + 3) <> 0xA4 then
    failwith "verify_padding"

let command_buf ~code ~addr ~ans_size =
  let buf = IO.output_string () in
  IO.write_byte buf code;
  IO.write_ui16 buf addr;
  IO.write_ui16 buf ans_size;
  IO.close_out buf

let fully_received ans =
  let n = String.length ans in
  n > 2 && ans.[n - 2] == '\x00' && ans.[n - 1] == '\xFF'

let run_command port ~code ~addr ~ans_size =
  Ser_port.write port (command_buf ~code ~addr ~ans_size);
  let ans = Ser_port.read port (ans_size + 2) in
  if not (fully_received ans) then
    raise (Invalid_response "end of response marker");
  String.sub ans 0 ans_size

let run_pkg_command port ~code ~addr ~ans_size =
  (* TODO: Use bytes *)
  let ans = String.make ans_size '\x00' in
  let pkg_size = 768 in
  let rec aux off =
    if off < ans_size then
      begin
        let part_size = min (ans_size - off) pkg_size in
        let part = run_command port ~code ~addr:(addr + off) ~ans_size:part_size in
        String.blit part 0 ans off part_size;
        aux (off + part_size)
      end in
  aux 0;
  ans

module Settings = Rox6_settings
module Totals = Rox6_totals

module Log_summary =
  struct
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
      verify_checksum bytea ~n:48;
      verify_padding bytea ~k:49;
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
          ((bytea.(26) lsl 8) lor bytea.(25)) - log_addr;
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

    let recv = decode % run_command ~code:0xEF ~addr:0x0071 ~ans_size:53
  end

module Bike_entry =
  struct
    type t = {
        ts : int;         (* s *)
        wheel_rot : int;
        duration : int;         (* s *)
        speed : float;          (* km/h *)
        cadence : int;          (* rpm *)
        hr : int;               (* bpm *)
        alt : float;            (* m *)
        temp : int;             (* °C *)

        distance : float;       (* m *)
        abs_distance : float;   (* m *)
        unadj_distance : float; (* m *)
      }

    type opt = Entry of t
             | Pause_entry of t
             | No_entry

    let size = 9

    (* Update timestamp field *)
    let fill_ts prev_entry entry =
      {entry with ts = entry.duration +
                         (match prev_entry with
                            Entry e -> e.ts
                          | Pause_entry e -> e.ts
                          | No_entry -> 0)}

    let scan wheel_circum prev_entry buf =
      let c = char_codes buf in
      let wheel_rot = ((c.(2) land 0x03) lsl 8) lor c.(1) in
      let unadj_distance =
        wheel_circum *. float_of_int wheel_rot in
      let distance =
        unadj_distance -.
          (match prev_entry with
             Pause_entry prev -> prev.unadj_distance
           | Entry _ | No_entry -> 0.0) in
      { ts = 0;                 (* Filled later *)
        wheel_rot;
        duration = sample_interval -
                     (match prev_entry with
                        Pause_entry prev -> prev.ts mod sample_interval
                      | Entry _ | No_entry -> 0);
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
        distance;
        abs_distance =
          distance +.
            (match prev_entry with
               No_entry -> 0.0
             | Entry prev | Pause_entry prev -> prev.abs_distance);
        unadj_distance
      } |> fill_ts prev_entry
  end

module Bike_lap =
  struct
    type t = {
        ts : int;               (* s *)
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

        distance : float;       (* m *)
        abs_distance : float;   (* m *)
      }

    type opt = Lap of t | No_lap

    let size = 23

    let scan wheel_circum prev_lap buf =
      let c = char_codes buf in
      let duration =
        ((c.(3) land 0x3F) lsl 16) lor (c.(2) lsl 8) lor c.(1) in
      let wheel_rot =
        (c.(8) lsl 16) lor (c.(7) lsl 8) lor c.(6) in
      let distance =
        wheel_circum *. float_of_int wheel_rot in
      { ts = duration + (match prev_lap with
                           Lap prev -> prev.ts
                         | No_lap -> 0 );
        wheel_rot;
        duration;
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
        distance;
        abs_distance = distance +.
                         (match prev_lap with
                            Lap prev -> prev.abs_distance;
                          | No_lap -> 0.0);
      }
  end

module Bike_pause =
  struct
    type t = {
        ts : int;               (* s *)
        wheel_rot : int;
        avg_alt : float;        (* m *)
        start_date : Date.t;
        start_time : Time.t;
        stop_date : Date.t;
        stop_time : Time.t;

        distance : float;       (* m *)
        abs_distance : float;   (* m *)
      }

    type opt = Pause of t | No_pause

    let size = 21

    let scan wheel_circum prev_entry prev_pause buf =
      let c = char_codes buf in
      let duration = c.(0) lsr 3 in
      let wheel_rot = ((c.(2) land 0x03) lsl 8) lor c.(1) in
      let distance = wheel_circum *. float_of_int wheel_rot in
      let abs_distance =
        distance +.
          (match prev_entry with
             Bike_entry.No_entry -> 0.0
           | Bike_entry.Entry e | Bike_entry.Pause_entry e -> e.Bike_entry.abs_distance) -.
          (match prev_entry with
             Bike_entry.Pause_entry e -> e.Bike_entry.unadj_distance
           | Bike_entry.No_entry | Bike_entry.Entry _ -> 0.0) in
      let entry =
        if duration == 0 then
          Bike_entry.No_entry
        else
          let tmp_entry =
            Bike_entry.scan wheel_circum prev_entry buf in
          Bike_entry.Entry (
              Bike_entry.fill_ts
                prev_entry {tmp_entry with
                             Bike_entry.duration =
                               duration -
                                 (match prev_entry with
                                    Bike_entry.Pause_entry e -> e.Bike_entry.duration
                                  | Bike_entry.No_entry | Bike_entry.Entry _ -> 0)}
            ) in
      let pause =
        { ts = duration +
                 (match prev_entry with
                    Bike_entry.No_entry -> 0
                  | Bike_entry.Entry e | Bike_entry.Pause_entry e -> e.Bike_entry.ts) -
                 (match entry with
                    Bike_entry.No_entry -> 0
                  | Bike_entry.Entry e | Bike_entry.Pause_entry e -> e.Bike_entry.duration);
          wheel_rot;
          avg_alt =
            begin
              let alt = float_of_int (((c.(8) land 0x1F) lsl 8) lor c.(7)) in
              if (c.(8) lsr 7) == 0 then
                alt
              else
                -.alt
            end;
          start_date =
            { Date.y = ((c.(9 + 2) land 0x0F) lsl 8) lor c.(8 + 2);
              mon = c.(9 + 2) lsr 4;
              d = c.(7 + 2) land 0x3F };
          start_time =
            { Time.h = c.(13 + 2) land 0x1F;
              min = ((c.(14 + 2) land 0xE0) lsr 2) lor (c.(13 + 2) lsr 5);
              s = c.(15 + 2) land 0x3F };
          stop_date =
            { Date.y = ((c.(12 + 2) land 0x0F) lsl 8) lor c.(11 + 2);
              mon = c.(12 + 2) lsr 4;
              d = c.(10 + 2) land 0x3F };
          stop_time =
            { Time.h = c.(14 + 2) land 0x1F;
              min = c.(16 + 2) land 0x3F;
              s = c.(17 + 2) land 0x3F };
          distance = abs_distance -.
                       (match prev_pause with
                          No_pause -> 0.0
                        | Pause p -> p.abs_distance);
          abs_distance;
        } in
      entry, pause
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
           | Bike_lap of Bike_lap.t
           | Bike_pause of Bike_pause.t
           | Hike of Hike_entry.t
           | Hike_pause of Hike_pause.t
  end

module Log =
  struct
    type t = Log_entry.t list

    type prev = {
        bike_entry : Bike_entry.opt;
        bike_lap : Bike_lap.opt;
        bike_pause : Bike_pause.opt;
      }

    let scan {Log_summary.wheel_circum; _} buf =
      let n = String.length buf in
      let rec aux k prev ans =
        if k < n then
          begin
            match (Char.code buf.[k]) land 0x07 with
            | 0 ->
               let e0 = String.sub buf k Bike_entry.size |>
                          Bike_entry.scan wheel_circum prev.bike_entry in
               let e1 = Log_entry.Bike e0 in
               aux (k + Bike_entry.size)
                   {prev with bike_entry = Bike_entry.Entry e0}
                   (e1 :: ans)
            | 1 ->
               let e0, m0 = String.sub buf k Bike_pause.size |>
                              Bike_pause.scan wheel_circum prev.bike_entry prev.bike_pause in
               let m1 = Log_entry.Bike_pause m0 in
               begin
                 match e0 with
                 | Bike_entry.Entry e0 | Bike_entry.Pause_entry e0 ->
                    let e1 = Log_entry.Bike e0 in
                    aux (k + Bike_pause.size)
                        {prev with bike_entry = Bike_entry.Pause_entry e0;
                                   bike_pause = Bike_pause.Pause m0}
                        (m1 :: e1 :: ans)
                 | Bike_entry.No_entry ->
                    aux (k + Bike_pause.size)
                        {prev with bike_pause = Bike_pause.Pause m0}
                        (m1 :: ans)
               end
            | 2 ->
               let m0 = String.sub buf k Bike_lap.size |>
                          Bike_lap.scan wheel_circum prev.bike_lap in
               let m1 = Log_entry.Bike_lap m0 in
               aux (k + Bike_lap.size)
                   {prev with bike_lap = Bike_lap.Lap m0}
                   (m1 :: ans)
            | 3 ->
               let e1 = Log_entry.Hike (
                            String.sub buf k Hike_entry.size |> Hike_entry.scan
                          ) in
               aux (k + Hike_entry.size)
                   prev
                   (e1 :: ans)
            | 4 ->
               let m1 = Log_entry.Hike_pause (
                            String.sub buf k Hike_pause.size |> Hike_pause.scan
                          ) in
               aux (k + Hike_pause.size)
                   prev
                   (m1 :: ans)
            | _ -> raise (Invalid_response "log entry type")
          end
        else
          begin
            (* TODO: Ignore last decoded entry if it's a pause. *)
            let ans = List.rev ans in
            (* Insert fake first entry with zeroes for time and distance. *)
            let ans =
              match ans with
              | (Log_entry.Bike e) :: _rest ->
                 (Log_entry.Bike {e with Bike_entry.ts = 0;
                                         wheel_rot = 0;
                                         duration = 0;
                                         speed = 0.0;
                                         distance = 0.0;
                                         abs_distance = 0.0}) :: ans
              | (Log_entry.Bike_lap _) :: _ -> ans
              | (Log_entry.Bike_pause _) :: _ -> ans
              | (Log_entry.Hike _) :: _ -> ans
              | (Log_entry.Hike_pause _) :: _ -> ans
              | [] -> ans in
            ans
          end in
      aux 0 {bike_entry = Bike_entry.No_entry;
             bike_lap = Bike_lap.No_lap;
             bike_pause = Bike_pause.No_pause} []
  end

module Bat_low =
  struct
    let scan ans =
      let c = char_codes ans in
      if (Array.(sub c 0 6 |> sum) land 0x0F) != (c.(6) lsr 4) then
        raise (Invalid_response "checksum");
      (c.(2) land 0x80) != 0
  end

let log port ({Log_summary.log_size; _} as summary) =
  run_pkg_command port ~code:0xEF ~addr:log_addr ~ans_size:log_size |> Log.scan summary

let bat_low =
  Bat_low.scan % run_command ~code:0xEF ~addr:0x006A ~ans_size:7
