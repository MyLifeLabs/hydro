(* $Id$ *)

(* This is the proxy view on Ice::LocalObject *)

(* A proxy view on such object does not exist *)

class type od_Ice_LocalObject =
object
end

class type oi_Ice_LocalObject =
object
  method hydro_invoke_operation : 
    string -> Hydro_types.value array -> Hydro_types.session -> unit
  method hydro_effective_id :
    string
end

class type o_Ice_LocalObject =
object
  inherit od_Ice_LocalObject
  inherit oi_Ice_LocalObject
end

class delegate_od_Ice_LocalObject : #od_Ice_LocalObject -> od_Ice_LocalObject

class delegate_oi_Ice_LocalObject : #oi_Ice_LocalObject -> oi_Ice_LocalObject

class mk_od_Ice_LocalObject : unit -> od_Ice_LocalObject

val dispatch_Ice_LocalObject : 
  oi_Ice_LocalObject -> string -> Hydro_types.value array -> 
  Hydro_types.session ->
  unit

class skel_Ice_LocalObject : oi_Ice_LocalObject

val fill_system : Hydro_types.system -> unit

