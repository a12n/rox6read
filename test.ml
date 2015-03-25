#load "_build/ssfc.cma";;
#require "tcx";;

open Batteries

let z = input_file "test/rox_6-settings.dat" |> Rox6.Settings.decode
let t = input_file "test/rox_6-totals.dat" |> Rox6.Totals.decode
let s = input_file "test/rox_6-log_summary.dat" |> Rox6.Log_summary.decode
let l = input_file "test/rox_6-log.dat" |> Rox6.Log.decode s

let ts_of_entry = function
    Rox6.Log_entry.Bike {Rox6.Bike_entry.ts; _} -> ts
  | Rox6.Log_entry.Bike_lap {Rox6.Bike_lap.ts; _} -> ts
  | Rox6.Log_entry.Bike_pause {Rox6.Bike_pause.ts; _} -> ts
  | Rox6.Log_entry.Hike _ | Rox6.Log_entry.Hike_pause _ -> failwith "Hike"

let l = List.sort (fun a b -> compare (ts_of_entry a) (ts_of_entry b)) l

let start_time =
  let {Rox6.Log_summary.start_date = {Date.y; mon; d};
       start_time = {Time.h; min; s}; _} = s in
  let tz = {Tcx.Time_zone.hours = 3; minutes = 0} in
  {Tcx.Timestamp.date = {Tcx.Date.year = y; month = mon; day = d};
   time = {Tcx.Time.hour = h; minute = min; second = s};
   time_zone = Some tz}
  |> Tcx.Timestamp.to_unix_time

let mps_of_kmh kmh = kmh *. 1000.0 /. 3600.0

let rec collect start_time laps tracks track_points = function
  | [] -> {Tcx.Activity.id = Tcx.Timestamp.of_unix_time start_time;
           sport = Tcx.Sport.Biking;
           laps = List_ext.Non_empty.of_list (List.rev laps);
           notes = None;
           creator = None}
  | (Rox6.Log_entry.Bike b) :: rest ->
     let {Rox6.Bike_entry.ts; alt;
          abs_distance; hr; cadence; _} = b in
     let track_point =
       {Tcx.Track_point.time =
          Tcx.Timestamp.of_unix_time (start_time +. float_of_int ts);
        position = None;
        altitude = Some alt;
        distance = Some abs_distance;
        heart_rate = Some hr;
        cadence = Some cadence;
        sensor_state = None} in
     collect start_time laps tracks (track_point :: track_points) rest
  | (Rox6.Log_entry.Bike_pause p) :: rest ->
     let track = {Tcx.Track.points = List_ext.Non_empty.of_list (List.rev track_points)} in
     collect start_time laps (track :: tracks) [] rest
  | (Rox6.Log_entry.Bike_lap l) :: rest ->
     let {Rox6.Bike_lap.ts; duration; distance; avg_hr;
          max_hr; max_speed; kcal; avg_cadence; _} = l in
     let track = {Tcx.Track.points = List_ext.Non_empty.of_list (List.rev track_points)} in
     let lap = {Tcx.Activity_lap.start_time =
                  Tcx.Timestamp.of_unix_time (start_time +. float_of_int (ts - duration));
                total_time = float_of_int duration;
                distance;
                maximum_speed = Some (mps_of_kmh max_speed);
                maximum_heart_rate = Some max_hr;
                average_heart_rate = Some avg_hr;
                calories = kcal;
                intensity = Tcx.Intensity.Active;
                cadence = Some avg_cadence;
                trigger_method = Tcx.Trigger_method.Manual;
                tracks = List.rev (track :: tracks);
                notes = None} in
     collect start_time (lap :: laps) [] [] rest
  | (Rox6.Log_entry.Hike _) :: _ -> failwith "Rox6.Log_entry.Hike"
  | (Rox6.Log_entry.Hike_pause _) :: _ -> failwith "Rox6.Log_entry.Hike_pause"

let to_tcx entries =
  let activity = collect start_time [] [] [] entries in
  {Tcx.activities = [activity]; author = None}
