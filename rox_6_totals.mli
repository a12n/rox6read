type t = {
    distance : float * float;
    duration : int * int;
    alt_gain : float * float;
    kcal : int * int;
    hike_duration : int;
    hike_alt_gain : float;
    hike_kcal : int;
  }

val recv : Ser_port.t -> t
