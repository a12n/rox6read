exception Unknown_device

let command port ~code ~ans_size =
  Ser_port.write port (String.make 1 (Char.chr code));
  Ser_port.read port ans_size

let device_connected port =
  let ans = command port ~code:0xF4 ~ans_size:1 in
  ans.[0] == '\x01'

let device_info port =
  let ans = command port ~code:0xFE ~ans_size:11 in
  match ans.[1] with
    '\x00' -> None
  | '\x17' ->
     Some { Device_info.model = Device_model.Rox_5;
            ser_number = String.sub ans 2 4 }
  | '\x18' ->
     Some { Device_info.model = Device_model.Rox_6;
            ser_number = String.sub ans 2 4 }
  | _ -> raise Unknown_device
