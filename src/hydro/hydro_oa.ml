open Hydro_types


exception Identity_exists of identity
exception Object_exists of Hydro_lm.interface_base 


class type object_adapter_t =
object
  method add : identity -> Hydro_lm.interface_base -> unit
  method mem_id : identity -> bool
  method mem_obj : Hydro_lm.interface_base -> bool
  method remove_id : identity -> unit
  method remove_obj : Hydro_lm.interface_base -> unit

  method invoke_object : identity -> facet_dispatcher
  method get_identity : Hydro_lm.interface_base -> identity

  method adapter_id : string option
  method set_adapter_id : string option -> unit
  method replica_group_id : string option
  method set_replica_group_id : string option -> unit
end


class object_adapter() : object_adapter_t =
  let id_of_obj = Hashtbl.create 10 in
  let obj_of_id = Hashtbl.create 10 in
object(self)
  val mutable adapter_id = None
  val mutable replica_group_id = None
  
  method add id obj =
    let id_name = id#name in
    let id_cat = id#category in
    if Hashtbl.mem obj_of_id (id_name,id_cat) then
      raise(Identity_exists id);
    if Hashtbl.mem id_of_obj obj then
      raise(Object_exists obj);
    Hashtbl.add obj_of_id (id_name,id_cat) obj;
    Hashtbl.add id_of_obj obj (id_name,id_cat)

  method mem_id id =
    let id_name = id#name in
    let id_cat = id#category in
    Hashtbl.mem obj_of_id (id_name,id_cat)

  method mem_obj obj =
    Hashtbl.mem id_of_obj obj

  method remove_id id =
    try
      let id_name = id#name in
      let id_cat = id#category in
      let obj = Hashtbl.find obj_of_id (id_name,id_cat) in (* or Not_found *)
      Hashtbl.remove obj_of_id (id_name,id_cat);
      Hashtbl.remove id_of_obj obj
    with
      | Not_found -> ()

  method remove_obj obj =
    try
      let (id_name,id_cat) = Hashtbl.find id_of_obj obj in (* or Not_found *)
      Hashtbl.remove obj_of_id (id_name,id_cat);
      Hashtbl.remove id_of_obj obj
    with
      | Not_found -> ()

  method invoke_object id =
    let id_name = id#name in
    let id_cat = id#category in
    let obj = Hashtbl.find obj_of_id (id_name,id_cat) in (* or Not_found *)
    ( object
	method invoke_facet f_opt =
	  match f_opt with
	    | None -> (* default facet *)
		(obj :> operation_dispatcher)
	    | _ ->
		raise Not_found
      end
    )

  method get_identity obj =
    let (id_name,id_cat) = Hashtbl.find id_of_obj obj in (* or Not_found *)
    ( object
	method name = id_name
	method category = id_cat
      end
    )

  method adapter_id = adapter_id
  method set_adapter_id id = adapter_id <- id

  method replica_group_id = replica_group_id
  method set_replica_group_id id = replica_group_id <- id

end


let object_adapter = new object_adapter
