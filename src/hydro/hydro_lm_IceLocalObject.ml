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
  (* Note: Operations are unimplemented *)
  inherit od_Ice_LocalObject
  inherit oi_Ice_LocalObject
end

class delegate_od_Ice_LocalObject (_ : #od_Ice_LocalObject) =
object
end

class delegate_oi_Ice_LocalObject oi : oi_Ice_LocalObject =
object
  method hydro_invoke_operation = oi#hydro_invoke_operation
  method hydro_effective_id = oi#hydro_effective_id
end

class mk_od_Ice_LocalObject () =
object
end

let dispatch_Ice_LocalObject _ _ =
  raise Not_found

class skel_Ice_LocalObject : oi_Ice_LocalObject =
object(self)
  method hydro_invoke_operation =
    dispatch_Ice_LocalObject self
  method hydro_effective_id =
    "::Ice::LocalObject"
end

let fill_system _ = ()
