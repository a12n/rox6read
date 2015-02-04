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

module Bat_status =
  struct
    type t = Ok | Low

    let scan _ans =
      (* TODO *)
      Low

    let recieve =
      scan % command ~code:0xEF ~address:0x6A00 ~ans_size:7
  end
