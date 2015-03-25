#load "_build/ssfc.cma";;

open Batteries

let z = input_file "test/rox_6-settings.dat" |> Rox_6.Settings.scan
let t = input_file "test/rox_6-totals.dat" |> Rox_6.Totals.scan
let s = input_file "test/rox_6-log_summary.dat" |> Rox_6.Log_summary.scan
let l = input_file "test/rox_6-log.dat" |> Rox_6.Log.scan s

let ts_of_entry = function
    Rox_6.Log_entry.Bike {Rox_6.Bike_entry.ts; _} -> ts
  | Rox_6.Log_entry.Bike_lap {Rox_6.Bike_lap.ts; _} -> ts
  | Rox_6.Log_entry.Bike_pause {Rox_6.Bike_pause.ts; _} -> ts
  | Rox_6.Log_entry.Hike _ | Rox_6.Log_entry.Hike_pause _ -> failwith "Hike"

let l = List.sort (fun a b -> compare (ts_of_entry a) (ts_of_entry b)) l

let start_time =
  let {Rox_6.Log_summary.start_date = {Date.y; mon; d};
       start_time = {Time.h; min; s}; _} = s in
  let tz = {Tcx.Time_zone.hours = 3; minutes = 0} in
  {Tcx.Timestamp.date = {Tcx.Date.year = y; month = mon; day = d};
   time = {Tcx.Time.hour = h; minute = min; second = s};
   time_zone = Some tz}
  |> Tcx.Timestamp.to_unix_time

let rec collect_track_points start_time ans = function
  | (Rox_6.Log_entry.Bike {Rox_6.Bike_entry.ts; alt;
                           abs_distance; hr; cadence; _}) :: rest ->
     let track_point =
       {Tcx.Track_point.time =
          Tcx.Timestamp.of_unix_time (start_time +. float_of_int ts);
        position = None;
        altitude = Some alt;
        distance = Some abs_distance;
        heart_rate = Some hr;
        cadence = Some cadence;
        sensor_state = None} in
     collect_track_points start_time (track_point :: ans) rest
  | rest -> List.rev ans, rest

let rec collect_tracks start_time ans = function
  | (Rox_6.Log_entry.Bike_pause _) :: rest ->
     collect_tracks start_time ans rest
  | ((Rox_6.Log_entry.Bike_lap _) :: _) as list -> List.rev ans, list
  | [] -> List.rev ans, []
  | ((Rox_6.Log_entry.Bike _) :: _) as list ->
     let track_points, rest = collect_track_points start_time [] list in
     let track = {Tcx.Track.points = List_ext.Non_empty.of_list track_points} in
     collect_tracks start_time (track :: ans) rest
  | (Rox_6.Log_entry.Hike _) :: _ -> failwith "Rox_6.Log_entry.Hike"
  | (Rox_6.Log_entry.Hike_pause _) :: _ -> failwith "Rox_6.Log_entry.Hike_pause"

let mps_of_kmh kmh = kmh *. 1000.0 /. 3600.0

let rec collect start_time laps tracks track_points = function
  | [] -> {Tcx.Activity.id = Tcx.Timestamp.of_unix_time start_time;
           sport = Tcx.Sport.Biking;
           laps = List_ext.Non_empty.of_list (List.rev laps);
           notes = None;
           creator = None}
  | (Rox_6.Log_entry.Bike b) :: rest ->
     let {Rox_6.Bike_entry.ts; alt;
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
  | (Rox_6.Log_entry.Bike_pause p) :: rest ->
     let track = {Tcx.Track.points = List_ext.Non_empty.of_list (List.rev track_points)} in
     collect start_time laps (track :: tracks) [] rest
  | (Rox_6.Log_entry.Bike_lap l) :: rest ->
     let {Rox_6.Bike_lap.ts; duration; distance; avg_hr;
          max_hr; max_speed; kcal; avg_cadence; _} = l in
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
                tracks = List.rev tracks;
                notes = None} in
     collect start_time (lap :: laps) [] [] rest
  | (Rox_6.Log_entry.Hike _) :: _ -> failwith "Rox_6.Log_entry.Hike"
  | (Rox_6.Log_entry.Hike_pause _) :: _ -> failwith "Rox_6.Log_entry.Hike_pause"

let to_tcx entries =
  let activity = collect start_time [] [] [] entries in
  {Tcx.activities = [activity]; author = None}
