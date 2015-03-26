open Batteries

type t = (float * float) array

let lerp (a, b) t = a +. t *. (b -. a)

let interpolate k (k1, v1) (k2, v2) =
  let t = (k -. k1) /. (k2 -. k1) in lerp (v1, v2) t

let eval_opt f x =
  let cmp (k1, _v1) (k2, _v2) =
    (BatOrd.ord compare) k1 k2 in
  match Array.bsearch cmp f (x, 0.0) with
  | `All_lower | `All_bigger | `Empty -> None
  | `At k -> Some (snd f.(k))
  | `Just_after k -> Some (interpolate x f.(k) f.(k + 1))

let eval f x = Option.default 0.0 (eval_opt f x)

let of_array = Array.decorate_stable_sort fst

let samples f (min_x, max_x) dx =
  let next x =
    if x <= max_x then
      eval f x, x +. dx
    else
      raise Enum.No_more_elements in
  Enum.from_loop min_x next |> Array.of_enum
