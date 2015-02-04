open Batteries

let format_command ~code ~address ~ans_size =
  let output = IO.output_string () in
  IO.write_byte output code;
  IO.BigEndian.write_ui16 output address;
  IO.write_ui16 output ans_size;
  IO.close_out output

let fully_received ans =
  let n = String.length ans in
  n > 2 && ans.[n - 2] == '\x00' && ans.[n - 1] == '\xFF'

let command ds ~code ~address ~ans_size =
  let ans = Dock.command ds ~data:(format_command ~code ~address ~ans_size)
                         ~ans_size:(ans_size + 2) in
  if not (fully_received ans) then
    failwith "Missing end of response marker";
  String.sub ans 0 ans_size

let char_codes =
  Array.of_list % List.map Char.code % String.to_list

module Bat_status =
  struct
    type t = Ok | Low

    let scan ans =
      (* TODO: Checksum *)
      if (Char.code ans.[2]) land 0x80 == 0 then
        Ok
      else
        Low

    let recieve =
      scan % command ~code:0xEF ~address:0x6A00 ~ans_size:7
  end

module Settings =
  struct
    (* TODO *)
    type t = unit

    let scan _ans =
      (* TODO *)
      ()

    let recieve =
      scan % command ~code:0xEF ~address:0x2000 ~ans_size:34
  end

module Totals =
  struct
    type t = {
        distance : int * int;
        time : int * int;
        cal : int * int;
        climb : int * int;
        hike_alt : int;
        hike_time : int;
        hike_cal : int;
      }

    let scan ans =
      let c = char_codes ans in
      (* TODO: Checksum *)
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
        cal = (
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
        hike_cal =
          (((c.(25) land 0x10) lsr 4) lsl 16) lor (c.(21) lsl 8) lor c.(20) ;
        hike_time =
          ((c.(25) land 0x03) lsl 24) lor (c.(24) lsl 16) lor (c.(23) lsl 8) lor c.(22) }

    let recieve =
      scan % command ~code:0xEF ~address:0x4200 ~ans_size:40
  end
