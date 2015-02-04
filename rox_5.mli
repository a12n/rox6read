module Bat_status :
sig
  type t = Ok | Low

  val recieve : Dock.t -> t
end

module Settings :
sig
  type t

  val recieve : Dock.t -> t
end

module Totals :
sig
  type t = {
      (* Bike *)
      distance : int * int;     (* m *)
      time : int * int;         (* s *)
      cal : int * int;          (* kcal *)
      climb : int * int;        (* mm *)
      (* Hike *)
      hike_alt : int;
      hike_time : int;
      hike_cal : int;
    }

  val recieve : Dock.t -> t
end
