open Batteries

(* Elevation data *)

let alt_data_of_gpx {Gpx.trk; _} =
  let to_tcx_time_zone = function
      Gpx.TIMEZONE_Z -> {Tcx.Time_zone.hours = 0; minutes = 0}
    | Gpx.TIMEZONE_plus (hours, minutes) -> {Tcx.Time_zone.hours; minutes}
    | Gpx.TIMEZONE_minus (hours, minutes) -> {Tcx.Time_zone.hours = -hours; minutes} in
  let to_tcx_timestamp {Gpx.year; month; day; hour; minute; second; timezone} =
    {Tcx.Timestamp.date = {Tcx.Date.year; month; day};
     time = {Tcx.Time.hour; minute; second = int_of_float second};
     time_zone = Option.map to_tcx_time_zone timezone} in
  let to_unix_time gpx_ts =
    Tcx.Timestamp.to_unix_time (to_tcx_timestamp gpx_ts) in
  let of_wpt ans = function
    | {Gpx.time = Some t; ele = Some h; _} -> (to_unix_time t, h) :: ans
    | _ -> ans in
  let of_trkseg {Gpx.trkpt; _} =
    trkpt |> List.fold_left of_wpt [] |> List.rev in
  let of_trk {Gpx.trkseg; _} =
    trkseg |> List.map of_trkseg |> List.flatten in
  trk |> List.map of_trk |> List.flatten |> Array.of_list

let alt_data_of_tcx tcx =
  let f ans = function
    | `Track_point {Tcx.Track_point.time; altitude = Some h; _} ->
       (Tcx.Timestamp.to_unix_time time, h) :: ans
    | _ -> ans in
  Tcx.fold f [] tcx |> List.rev |> Array.of_list

(* Main *)

let () =
  (* TODO *)
  ()
