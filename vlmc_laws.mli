(** Registering laws *)

val laws : (module Vlmc.Law) list ref
val register_law : (module Vlmc.Law) -> unit