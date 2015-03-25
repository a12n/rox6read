type t = {
    start_date : Date.t;        (**  *)
    start_time : Time.t;

    (** Rider's age (years). *)
    age : int;
    (** Rider's body mass (kilograms). *)
    mass : float;
    (** Rider's sex. *)
    sex : Sex.t;

    (** Rider's maximum heart rate (beats per minute). *)
    max_hr : int;
    (** Lower and upper heart rate limits of rider's own training zone
     * (beats per minute). *)
    hr_limits : int * int;

    training_zone : Training_zone.t;
    zone_start : float * float * float * float; (* frac of max_hr *)

    bike_no : Bike_no.t;
    wheel_circum : float;     (* m *)

    distance : float;         (* m *)
    duration : int;           (* s *)
    max_speed : float * int;  (* km/h, log entry index *)
    alt_gain : float;         (* m *)
    alt_loss : float;         (* m *)
    kcal : int;               (* kcal *)

    hike_duration : int;      (* s *)
    hike_alt_gain : float;    (* m *)
    hike_alt_loss : float;    (* m *)
    hike_kcal : int;          (* kcal *)

    speed_unit : Speed_unit.t;
    mass_unit : Mass_unit.t;

    log_size : int;        (* size of log in device memory *)
  }

val recv : Ser_port.t -> t
