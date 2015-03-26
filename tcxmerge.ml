open Batteries

let error msg =
  prerr_string "tcxmerge: ";
  prerr_endline msg;
  exit 1

(* Collect data points of GPX/TCX tracks *)

let data_of_gpx f {Gpx.trk; _} =
  let of_wpt ans = function
    | {Gpx.time = Some (t, _tz); ele = _; _} as p ->
       let data = f p in
       if Option.is_some data then
         (t, Option.get data) :: ans
       else
         ans
    | _ -> ans in
  let of_trkseg {Gpx.trkpt; _} =
    trkpt |> List.fold_left of_wpt [] |> List.rev in
  let of_trk {Gpx.trkseg; _} =
    trkseg |> List.map of_trkseg |> List.flatten in
  trk |> List.map of_trk |> List.flatten |> Array.of_list

let data_of_tcx f tcx =
  let collect ans = function
    | `Track_point ({Tcx.Track_point.time; _} as p) ->
       let data = f p in
       if Option.is_some data then
         (Tcx.Timestamp.to_unix_time time, Option.get data) :: ans
       else
         ans
    | _ -> ans in
  Tcx.fold collect [] tcx |> List.rev |> Array.of_list

let alt_data_of_gpx = data_of_gpx (fun p -> p.Gpx.ele)

let alt_data_of_tcx = data_of_tcx (fun p -> p.Tcx.Track_point.altitude)

let lat_data_of_gpx = data_of_gpx (fun p -> Some p.Gpx.lat)

let lon_data_of_gpx = data_of_gpx (fun p -> Some p.Gpx.lon)

(* Cross correlation on elevation data *)

let xcorr_alt _tcx _gpx =
  (* TODO *)
  None

(* Merge data sets *)

let merge_data tcx gpx ?(time_lag=0.0) =
  let alt_data = Real_fun.of_array (alt_data_of_gpx gpx) in
  let lat_data = Real_fun.of_array (lat_data_of_gpx gpx) in
  let lon_data = Real_fun.of_array (lon_data_of_gpx gpx) in
  let transform = function
    | `Activity_lap ({Tcx.Activity_lap.start_time; _} as l) ->
       let t = Tcx.Timestamp.to_unix_time start_time +. time_lag in
       `Activity_lap {l with Tcx.Activity_lap.start_time = Tcx.Timestamp.of_unix_time t}
    | `Track_point ({Tcx.Track_point.time; altitude; _} as p) ->
       let t = Tcx.Timestamp.to_unix_time time +. time_lag in
       (* Use altitude from GPX track, if there's any. Otherwise use
        * altitude from TCX. *)
       let altitude =
         match Real_fun.eval_opt alt_data t with
           Some alt -> Some alt
         | None -> altitude in
       let position =
         match Real_fun.eval_opt2 lat_data lon_data t with
           Some (lat, lon) -> Some {Tcx.Position.latitude = lat; longitude = lon}
         | None -> None in
       `Track_point {p with Tcx.Track_point.time = Tcx.Timestamp.of_unix_time t;
                            altitude; position}
    | x -> x in
  Tcx.map transform tcx

(* GPX and TCX files I/O *)

let load_tcx path =
  try Tcx.parse_file path
  with _ -> error ("couldn't parse TCX file \"" ^ path ^ "\"")

let load_gpx path =
  try Gpx.of_xml (Xml.parse_file path)
  with _ -> error ("couldn't parse GPX file \"" ^ path ^ "\"")

(* Command line args *)

let parse_args () =
  let tcx_path = ref "in.tcx" in
  let gpx_path = ref "track.gpx" in
  Arg.parse
    [ "-tcx", Arg.Set_string tcx_path,
      "Path to input TCX file (default \"" ^ !tcx_path ^ "\")"
    ; "-gpx", Arg.Set_string gpx_path,
      "Path to GPX file (default \"" ^ !gpx_path ^ "\")" ]
    (fun _anon -> ())
    "Merge TCX data (heart rate, cadence) with GPS data from a GPX file";
  !tcx_path, !gpx_path

(* Main *)

let () =
  let tcx_path, gpx_path = parse_args () in
  let tcx = load_tcx tcx_path in
  let gpx = load_gpx gpx_path in
  let time_lag = xcorr_alt tcx gpx in
  merge_data tcx gpx ?time_lag |> Tcx.to_string |> print_string
