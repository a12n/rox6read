(* Formatting functions *)

let training_zone_to_string =
  function Training_zone.Fit -> "Fit"
         | Training_zone.Fat -> "Fat"
         | Training_zone.Own -> "Own"

let bool_to_string =
  function false -> "No"
         | true -> "Yes"

let sex_to_string =
  function Sex.Male -> "Male"
         | Sex.Female -> "Female"

let speed_unit_to_string =
  function Speed_unit.Kmh -> "km/h"
         | Speed_unit.Mph -> "mph"

let mass_unit_to_string =
  function Mass_unit.Kg -> "kg"
         | Mass_unit.Lb -> "lb"

let date_to_string {Date.y; mon; d} =
  Printf.sprintf "%04d-%02d-%02d" y mon d

let time_to_string {Time.h; min; s} =
  Printf.sprintf "%02d:%02d:%02d" h min s

(* Battery status *)

let read_battery port =
  Printf.printf "Battery: %s\n"
                (match Rox6.Bat_status.recv port with
                   Rox6.Bat_status.Ok -> "OK"
                 | Rox6.Bat_status.Low -> "Low")

(* Ride log *)

let ts_of_entry = function
    Rox6.Log_entry.Bike {Rox6.Bike_entry.ts; _} -> ts
  | Rox6.Log_entry.Bike_lap {Rox6.Bike_lap.ts; _} -> ts
  | Rox6.Log_entry.Bike_pause {Rox6.Bike_pause.ts; _} -> ts
  | Rox6.Log_entry.Hike _ | Rox6.Log_entry.Hike_pause _ -> failwith "ts_of_entry"

let remove_hike_entries =
  List.filter (function Rox6.Log_entry.Bike _ -> true
                      | Rox6.Log_entry.Bike_lap _ -> true
                      | Rox6.Log_entry.Bike_pause _ -> true
                      | Rox6.Log_entry.Hike _ -> false
                      | Rox6.Log_entry.Hike_pause _ -> false)

let sort_entries =
  List.sort (fun a b -> compare (ts_of_entry a) (ts_of_entry b))

let read_log port =
  let summary = Rox6.Log_summary.recv port in
  let log = Rox6.Log.recv port summary |> remove_hike_entries |> sort_entries in
  print_endline "TODO"

(* Settings *)

let read_settings port =
  let {Rox6.Settings.age; mass; sex; max_hr; hr_limits; training_zone;
       zone_alarm; zone_start = z1, z2, z3, z4; wheel_circum; date;
       time; slp; actual_alt; home_alt; alt_ref; lang; date_format;
       speed_unit; mass_unit; contrast; low_bat; serv_interval} =
    Rox6.Settings.recv port in
  Printf.(printf "Age: %d y\n" age;
          printf "Mass: %.3f kg\n" mass;
          printf "Sex: %s\n" (sex_to_string sex);
          printf "Max. Heart Rate: %d bpm\n" max_hr;
          printf "Lower Heart Rate Limit: %d bpm\n" (fst hr_limits);
          printf "Upper Heart Rate Limit: %d bpm\n" (snd hr_limits);
          printf "Training Zone: %s\n" (training_zone_to_string training_zone);
          printf "Zone Alarm: %s\n" (bool_to_string zone_alarm);
          printf "Zone 1 Start: %d %%\n" (int_of_float (z1 *. 100.0));
          printf "Zone 2 Start: %d %%\n" (int_of_float (z2 *. 100.0));
          printf "Zone 3 Start: %d %%\n" (int_of_float (z3 *. 100.0));
          printf "Zone 4 Start: %d %%\n" (int_of_float (z4 *. 100.0));
          printf "Bike 1 Wheel Circum.: %.3f m\n" (fst wheel_circum);
          printf "Bike 2 Wheel Circum.: %.3f m\n" (snd wheel_circum);
          printf "Date: %s\n" (date_to_string date);
          printf "Time: %s\n" (time_to_string time);
          printf "Sea Level Pressure: %d Pa\n" slp;
          printf "Actual Altitude: %.2f m\n" actual_alt;
          printf "Home Altitude: %.2f m\n" home_alt;
          printf "Altitude Reference: %s\n" Alt_ref.(match alt_ref with
                                                       Slp -> "Sea Level Pressure"
                                                     | Actual_alt -> "Actual Altitude");
          printf "Language: %s\n" Lang.(match lang with
                                          De -> "de"
                                        | En -> "en"
                                        | Es -> "es"
                                        | Fr -> "fr"
                                        | It -> "it"
                                        | Nl -> "nl"
                                        | Pl -> "pl");
          printf "Date Format: %s\n" Date_format.(match date_format with
                                                    Eu -> "European"
                                                  | Us -> "US");
          printf "Speed Unit: %s\n" (speed_unit_to_string speed_unit);
          printf "Mass Unit: %s\n" (mass_unit_to_string mass_unit);
          printf "Contrast: %s\n" Contrast.(match contrast with
                                              Low -> "Low"
                                            | Mid -> "Mid"
                                            | High -> "High");
          printf "Low Battery Level: %d mV\n" Low_bat.(match low_bat with
                                                         Below_1830 -> 1830
                                                       | Below_2000 -> 2000
                                                       | Below_2170 -> 2170
                                                       | Below_2330 -> 2330
                                                       | Below_2500 -> 2500
                                                       | Below_2670 -> 2670
                                                       | Below_2830 -> 2830
                                                       | Below_3000 -> 3000);
          printf "Service Interval Enabled: %s\n" (bool_to_string (fst serv_interval));
          printf "Service Interval: %d ?\n" (snd serv_interval))

(* Ride log summary *)

let read_summary port =
  let {Rox6.Log_summary.start_date; start_time; age; mass; sex;
       max_hr; hr_limits; training_zone; zone_start = z1, z2, z3, z4; bike_no;
       wheel_circum; distance; duration; max_speed; alt_gain;
       alt_loss; kcal; hike_duration; hike_alt_gain; hike_alt_loss;
       hike_kcal; speed_unit; mass_unit; log_size} =
    Rox6.Log_summary.recv port in
  Printf.(printf "Start Date: %s\n" (date_to_string start_date);
          printf "Start Time: %s\n" (time_to_string start_time);
          printf "Age: %d y\n" age;
          printf "Mass: %.3f kg\n" mass;
          printf "Sex: %s\n" (sex_to_string sex);
          printf "Max. Heart Rate: %d bpm\n" max_hr;
          printf "Lower Heart Rate Limit: %d bpm\n" (fst hr_limits);
          printf "Upper Heart Rate Limit: %d bpm\n" (snd hr_limits);
          printf "Training Zone: %s\n" (training_zone_to_string training_zone);
          printf "Zone 1 Start: %d %%\n" (int_of_float (z1 *. 100.0));
          printf "Zone 2 Start: %d %%\n" (int_of_float (z2 *. 100.0));
          printf "Zone 3 Start: %d %%\n" (int_of_float (z3 *. 100.0));
          printf "Zone 4 Start: %d %%\n" (int_of_float (z4 *. 100.0));
          printf "Bike no.: %d\n" Bike_no.(match bike_no with
                                             Bike_1 -> 1
                                           | Bike_2 -> 2);
          printf "Wheel Circumference: %.3f m\n" wheel_circum;
          printf "Distance: %.2f m\n" distance;
          printf "Duration: %d s\n" duration;
          printf "Max. Speed: %.1f km/h\n" (fst max_speed);
          printf "Altitude Gain: %.2f m\n" alt_gain;
          printf "Altitude Loss: %.2f m\n" alt_loss;
          printf "Energy Expend.: %d kcal\n" kcal;
          printf "Hike Duration: %d s\n" hike_duration;
          printf "Hike Altitude Gain: %.2f m\n" hike_alt_gain;
          printf "Hike Altitude Loss: %.2f m\n" hike_alt_loss;
          printf "Hike Energy Expend.: %d kcal\n" hike_kcal;
          printf "Speed Unit: %s\n" (speed_unit_to_string speed_unit);
          printf "Mass Unit: %s\n" (mass_unit_to_string mass_unit);
          printf "Log Size: %d\n" log_size)

(* Total values *)

let read_totals port =
  let {Rox6.Totals.distance; duration; alt_gain; kcal; hike_duration;
       hike_alt_gain; hike_kcal} = Rox6.Totals.recv port in
  Printf.(printf "Bike 1 Altitude Gain: %.2f m\n" (fst alt_gain);
          printf "Bike 1 Distance: %.2f m\n" (fst distance);
          printf "Bike 1 Duration: %d s\n" (fst duration);
          printf "Bike 1 Energy Expend.: %d kcal\n" (fst kcal);
          printf "Bike 2 Altitude Gain: %.2f m\n" (snd alt_gain);
          printf "Bike 2 Distance: %.2f m\n" (snd distance);
          printf "Bike 2 Duration: %d s\n" (snd duration);
          printf "Bike 2 Energy Expend.: %d kcal\n" (snd kcal);
          printf "Hike Altitude Gain: %.2f m\n" hike_alt_gain;
          printf "Hike Duration: %d s\n" hike_duration;
          printf "Hike Energy Expend.: %d kcal\n" hike_kcal)

(* Main *)

let () =
  let port_path = ref "" in
  let read_func = ref read_summary in
  let usage_msg = "Read data from SIGMA ROX 6.0 cycling computer" in
  let options =
    [ "-d", Arg.Set_string port_path, " Serial port device path";
      "-w", Arg.Symbol (["battery"; "log"; "settings"; "summary"; "totals"],
                        function "battery" -> read_func := read_battery
                               | "log" -> read_func := read_log
                               | "settings" -> read_func := read_settings
                               | "summary" -> read_func := read_summary
                               | "totals" -> read_func := read_totals
                               | symbol -> raise (Arg.Bad symbol)),
      "  Which piece of information to read" ] in
  Arg.parse options (fun _anon -> ()) usage_msg;
  let port = Unix.handle_unix_error Ser_port.open_port !port_path in
  if Dock.device_connected port then
    (match Dock.device_info port with
       Some {Device_info.model = Device_model.Rox6; _} -> !read_func port
     | _other -> failwith "Device isn't a ROX 6.0 computer")
  else
    failwith "No device in the docking station"
