let read_battery port =
  Printf.printf "Battery: %s\n"
                (match Rox6.Bat_status.recv port with
                   Rox6.Bat_status.Ok -> "OK"
                 | Rox6.Bat_status.Low -> "Low")

let read_log port =
  print_endline "TODO"

let read_settings port =
  let {Rox6.Settings.age; mass; sex; max_hr; hr_limits; training_zone;
       zone_alarm; zone_start = z1, z2, z3, z4; wheel_circum; date = {Date.y; mon; d};
       time = {Time.h; min; s}; slp; actual_alt; home_alt; alt_ref; lang; date_format;
       speed_unit; mass_unit; contrast; low_bat; serv_interval} =
    Rox6.Settings.recv port in
  Printf.(printf "Age: %d y\n" age;
          printf "Mass: %.3f kg\n" mass;
          printf "Sex: %s\n" Sex.(match sex with
                                    Male -> "Male"
                                  | Female -> "Female");
          printf "Max. Heart Rate: %d bpm\n" max_hr;
          printf "Lower Heart Rate Limit: %d bpm\n" (fst hr_limits);
          printf "Upper Heart Rate Limit: %d bpm\n" (snd hr_limits);
          printf "Training Zone: %s\n" Training_zone.(match training_zone with
                                                        Fit -> "Fit"
                                                      | Fat -> "Fat"
                                                      | Own -> "Own");
          printf "Zone Alarm: %s\n" (if zone_alarm then "Yes" else "No");
          printf "Zone 1 Start: %d %%\n" (int_of_float (z1 *. 100.0));
          printf "Zone 2 Start: %d %%\n" (int_of_float (z2 *. 100.0));
          printf "Zone 3 Start: %d %%\n" (int_of_float (z3 *. 100.0));
          printf "Zone 4 Start: %d %%\n" (int_of_float (z4 *. 100.0));
          printf "Bike 1 Wheel Circum.: %.3f m\n" (fst wheel_circum);
          printf "Bike 2 Wheel Circum.: %.3f m\n" (snd wheel_circum);
          printf "Date: %04d-%02d-%02d\n" y mon d;
          printf "Time: %02d:%02d:%02d\n" h min s;
          printf "Sea Level Pressure: %d Pa\n" slp;
          printf "Actual Altitude: %.3f m\n" actual_alt;
          printf "Home Altitude: %.3f m\n" home_alt;
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
          printf "Speed Unit: %s\n" Speed_unit.(match speed_unit with
                                                  Kmh -> "km/h"
                                                | Mph -> "mph");
          printf "Mass Unit: %s\n" Mass_unit.(match mass_unit with
                                                Kg -> "kg"
                                              | Lb -> "lb");
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
          printf "Service Interval Enabled: %s\n" (if (fst serv_interval) then "Yes" else "No");
          printf "Service Interval: %d ?\n" (snd serv_interval))

let read_summary port =
  let {Rox6.Log_summary.start_date = {Date.y; mon; d};
       start_time = {Time.h; min; s}; age; mass; sex;
       max_hr; hr_limits; training_zone; zone_start = z1, z2, z3, z4; bike_no;
       wheel_circum; distance; duration; max_speed; alt_gain;
       alt_loss; kcal; hike_duration; hike_alt_gain; hike_alt_loss;
       hike_kcal; speed_unit; mass_unit; log_size} =
    Rox6.Log_summary.recv port in
  Printf.printf "Start Date: %04d-%02d-%02d\n
                 Start Time: %02d:%02d:%02d\n
                 Age: %d y\n
                 Mass: %f kg\n
                 Sex: %s\n
                 Max. Heart Rate: %d bpm\n
                 Lower Heart Rate Limit: %d bpm\n
                 Upper Heart Rate Limit: %d bpm\n
                 Training Zone: %s\n
                 Zone 1 Start: %d %%\n
                 Zone 2 Start: %d %%\n
                 Zone 3 Start: %d %%\n
                 Zone 4 Start: %d %%\n
                 Bike no.: %d\n
                 Wheel Circumference: %f m\n
                 Distance: %f m\n
                 Duration: %d s\n
                 Max. Speed: %f km/h\n
                 Altitude Gain: %f m\n
                 Altitude Loss: %f m\n
                 Energy Expend.: %d kcal\n
                 Hike Duration: %d s\n
                 Hike Altitude Gain: %f m\n
                 Hike Altitude Loss: %f m\n
                 Hike Energy Expend.: %d kcal\n
                 Speed Unit: %s\n
                 Mass Unit: %s\n
                 Log Size: %d\n"
                y mon d
                h min s
                age
                mass
                Sex.(match sex with
                       Male -> "Male"
                     | Female -> "Female")
                max_hr
                (fst hr_limits)
                (snd hr_limits)
                Training_zone.(match training_zone with
                                 Fit -> "Fit"
                               | Fat -> "Fat"
                               | Own -> "Own")
                (int_of_float (z1 *. 100.0))
                (int_of_float (z2 *. 100.0))
                (int_of_float (z3 *. 100.0))
                (int_of_float (z4 *. 100.0))
                Bike_no.(match bike_no with
                           Bike_1 -> 1
                         | Bike_2 -> 2)
                wheel_circum
                distance
                duration
                (fst max_speed)
                alt_gain
                alt_loss
                kcal
                hike_duration
                hike_alt_gain
                hike_alt_loss
                hike_kcal
                Speed_unit.(match speed_unit with
                              Kmh -> "km/h"
                            | Mph -> "mph")
                Mass_unit.(match mass_unit with
                             Kg -> "kg"
                           | Lb -> "lb")
                log_size

let read_totals port =
  let {Rox6.Totals.distance; duration; alt_gain; kcal; hike_duration;
       hike_alt_gain; hike_kcal} = Rox6.Totals.recv port in
  Printf.(printf "Bike 1 Altitude Gain: %.3f m\n" (fst alt_gain);
          printf "Bike 1 Distance: %.3f m\n" (fst distance);
          printf "Bike 1 Duration: %d s\n" (fst duration);
          printf "Bike 1 Energy Expend.: %d kcal\n" (fst kcal);
          printf "Bike 2 Altitude Gain: %.3f m\n" (snd alt_gain);
          printf "Bike 2 Distance: %.3f m\n" (snd distance);
          printf "Bike 2 Duration: %d s\n" (snd duration);
          printf "Bike 2 Energy Expend.: %d kcal\n" (snd kcal);
          printf "Hike Altitude Gain: %.3f m\n" hike_alt_gain;
          printf "Hike Duration: %d s\n" hike_duration;
          printf "Hike Energy Expend.: %d kcal\n" hike_kcal)

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
    let _ = Dock.device_info port in
    !read_func port
  else
    exit 1