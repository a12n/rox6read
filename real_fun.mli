(** Function from [float] to [float]. *)
type t

(** Defined domain of the function. [None] if the function is empty. *)
val domain : t -> (float * float) option

(** Same as [eval_opt], but returns zero for arguments out of defined
 * set. *)
val eval : t -> float -> float

val eval2 : t -> t -> float -> float * float

(** Evaluate function [f] at point [x]. Linear interpolation is used
 * for missing data points. None is returned for points out of defined
 * set. *)
val eval_opt : t -> float -> float option

val eval_opt2 : t -> t -> float -> (float * float) option

(** Construct function [f] out of array of pairs [(x, y)]. *)
val of_array : (float * float) array -> t

(** Sample function [f] in interval [(min_x, max_x)] with sampling interval
 * [dx], starting at [min_x]. *)
val samples : t -> float * float -> float -> float array
