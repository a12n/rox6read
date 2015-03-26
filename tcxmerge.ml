open Batteries

let error msg =
  prerr_string "tcxmerge: ";
  prerr_endline msg;
  exit 1

(* Collect data points of GPX/TCX tracks *)

let data_of_gpx f {Gpx.trk; _} =
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
    | {Gpx.time = Some t; ele = _; _} as p ->
       let data = f p in
       if Option.is_some data then
         (to_unix_time t, Option.get data) :: ans
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

(* Cross correlation on elevation data *)

let xcorr_alt _tcx _gpx =
  (* TODO *)
  None

(* Merge data sets *)

let merge_data tcx _gpx ?(time_lag=0.0) =
  (* TODO *)
  tcx

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
