module Gc : sig
  val full_major : unit -> unit
end

module Sys : sig
  val argv : array (string)
end
