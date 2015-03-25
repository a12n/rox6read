open Batteries

type t = {
    distance : float * float;
    duration : int * int;
    alt_gain : float * float;
    kcal : int * int;
    hike_duration : int;
    hike_alt_gain : float;
    hike_kcal : int;
  }

let decode buf =
  let byte = Bytea.of_bytes buf in
  Rox_6.verify_checksum byte ~n:35;
  Rox_6.verify_padding byte ~k:36;
  { distance =
      (let bike_1 =
         float_of_int (
             (byte.( 0) lor (byte.( 1) lsl 8) lor (byte.( 2) lsl 16) lor ((byte.( 3) land 0x0F) lsl 24)) +
               (((byte.( 7) land 0xFC) lsl 2) lor ((byte.( 3) land 0xF0) lsr 4)) / 1000
           ) in
       let bike_2 =
         float_of_int (
             (byte.( 8) lor (byte.( 9) lsl 8) lor (byte.(10) lsl 16) lor ((byte.(11) land 0x0F) lsl 24)) +
               (((byte.(15) land 0xFC) lsl 2) lor ((byte.(11) land 0x0F) lsr 4)) / 1000
           ) in
       bike_1, bike_2);
    duration =
      (let bike_1 =
         byte.(4) lor (byte.(5) lsl 8) lor (byte.(6) lsl 16) lor
           (byte.(7) land 0x03) lsl 24 in
       let bike_2 =
         ((byte.(15) land 3) lsl 24) lor (byte.(14) lsl 16) lor
           (byte.(13) lsl 8) lor byte.(12) in
       bike_1, bike_2);
    kcal =
      (let bike_1 =
         (((byte.(25) land 0x04) lsr 2) lsl 16) lor (byte.(17) lsl 8) lor byte.(16) in
       let bike_2 =
         (((byte.(25) land 0x08) lsr 3) lsl 16) lor (byte.(19) lsl 8) lor byte.(18) in
       bike_1, bike_2);
    alt_gain =
      (let bike_1 =
         float_of_int (
             (((byte.(28) land 0x0F) lsl 16) lor (byte.(27) lsl 8) lor byte.(26)) * 100 +
               10 * ((byte.(28) land 0xF0) lsr 4)
           ) /. 1000.0 in
       let bike_2 =
         float_of_int (
             (((byte.(31) land 0x0F) lsl 16) lor (byte.(30) lsl 8) lor byte.(29)) * 100 +
               10 * ((byte.(31) land 0xF0) lsr 4)
           ) /. 1000.0 in
       bike_1, bike_2);
    hike_alt_gain =
      float_of_int (
          (((byte.(34) land 15) lsl 16) land (byte.(33) lsl 8) lor byte.(32)) * 100 +
            10 * ((byte.(34) land 0xF0) lsr 4)
        ) /. 1000.0;
    hike_kcal =
      (((byte.(25) land 0x10) lsr 4) lsl 16) lor (byte.(21) lsl 8) lor byte.(20) ;
    hike_duration =
      ((byte.(25) land 0x03) lsl 24) lor (byte.(24) lsl 16) lor (byte.(23) lsl 8) lor byte.(22) }

let recv = decode % Rox_6.run_command ~code:0xEF ~addr:0x0042 ~ans_size:40
