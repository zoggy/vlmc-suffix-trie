(** *)

let laws = ref ([] : (module Vlmc.Law) list);;
let register_law law = laws := law :: !laws;;

