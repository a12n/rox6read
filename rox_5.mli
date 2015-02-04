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
  type t

  val recieve : Dock.t -> t
end
