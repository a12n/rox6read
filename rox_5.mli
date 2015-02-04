module Bat_status :
sig
  type t = Ok | Low

  val recieve : Dock.t -> t
end
