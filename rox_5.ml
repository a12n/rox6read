open Batteries

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
      scan % Dock.command ~code:0xEF ~address:0x6A00 ~ans_size:7
  end

module Settings =
  struct
    (* TODO *)
    type t = unit

    let scan _ans =
      (* TODO *)
      ()

    let recieve =
      scan % Dock.command ~code:0xEF ~address:0x2000 ~ans_size:34
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
      (* Checksums *)
      if (Array.(sub c 0 35 |> sum) land 0xFF) != c.(35) then
        failwith "Invalid totals checksum";
      if c.(36) != 0xA1 || c.(37) != 0xA2 ||
           c.(38) != 0xA3 || c.(39) != 0xA4 then
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
      scan % Dock.command ~code:0xEF ~address:0x4200 ~ans_size:40
  end
