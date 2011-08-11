open Hydro_types
open Hydro_prelim

type 't proxy_reference = proxy_addr

module Unsafe = struct
  let wrap_proxy p = p
  let unwrap_proxy p = p

  let of_proxy_reference (x : 't proxy_reference option) : Hydro_types.value =
    match x with
      | None -> VNull
      | Some pr -> VProxy pr

  let to_proxy_reference v =
    match v with
      | VProxy pr -> Some pr
      | VNull -> None
      | _ -> assert false
end


let pr_of_address = Unsafe.wrap_proxy
let pr_of_string s = pr_of_address(Hydro_string.proxy_addr_of_string s)


exception Invalid_coercion
exception Error of int

let error n = raise(Error n)

type client_response =
    Hydro_endpoint.Client.response


class type ['response] call_suspension_t =
object
  method scall : 'response
  method acall : ('response -> unit) -> unit
  method params : call_params
  method with_params : call_params -> 'response call_suspension_t
end


exception ExitEventHandling

class ['response] call_suspension
      twoway_call (map : Hydro_endpoint.Client.response -> 'response)  esys
    : ['response] call_suspension_t =
object(self)
  val cp = Hydro_params.call_params()

  method scall =
    let value = ref None in
    let active = ref true in
    self # acall
      (fun v ->
	 (* Got the response. First remember the result value... *)
	 value := Some v;
	 (* ...then enforce that we leave the Unixqueue. This stops any
            further event processing, and especially the timeout handler
            against idle connections
          *)
	 let g = Unixqueue.new_group esys in
	 Unixqueue.once esys g 0.0 (fun () ->
				      if !active then raise ExitEventHandling);
      );
    ( try
	Unixqueue.run esys
      with
	| ExitEventHandling -> ()
	| err -> active := false; raise err
    );
    active := false;
    ( match !value with
	| None -> assert false
	| Some(result) -> result
    )

  method acall f =
    twoway_call
      cp
      (fun r ->
	 f (map r)
      )

  method params = cp

  method with_params cp' =
    ( {< cp = cp' >} :
	'response #call_suspension_t :> 'response call_suspension_t )
end


let call_suspension = new call_suspension


let create_system() =
  let types = CiHashtbl.create 50 in
  let exceptions = CiHashtbl.create 50 in
  let interfaces = CiHashtbl.create 50 in
  let classes = CiHashtbl.create 50 in
  let ctors = CiHashtbl.create 50 in
  ( object
      method types = types
      method exceptions = exceptions
      method interfaces = interfaces
      method classes = classes
      method ctors = ctors
    end : Hydro_types.system
  )


class type sliced_base =
object
  method hydro_slices : Hydro_types.slice list
  method hydro_effective_id : string
end


class type object_base =
object
  inherit sliced_base
  method hydro_inflate : string -> Hydro_types.noreturn
  method hydro_invoke_operation : string -> value array -> session -> unit
end


class type interface_base =
object
   method hydro_effective_id : string
   method hydro_invoke_operation : string -> Hydro_types.value array -> Hydro_types.session -> unit
end


let value_of_object obj =
  VClass (ref (`Value (obj : #object_base :> object_value)))


let object_of_class_repr cr =
  match !cr with
    | `Value obj -> obj
    | `Placeholder _ ->
	raise(Unmarshal_error "Found an object `Placeholder")


let seq_to_option v =
  match v with
    | [| |] -> None
    | [| x |] -> Some x
    | _ -> failwith "Hydro_lm.seq_to_option: more than one value in seq"

let option_to_seq =
  function
    | None -> [| |]
    | Some x -> [| x |]

let dict_to_hashtbl l =
  let ht = Hashtbl.create (List.length l) in
  List.iter
    (fun (k,v) -> Hashtbl.replace ht k v)
    l;
  ht

let hashtbl_to_dict ht =
  Hashtbl.fold
    (fun k v l -> (k,v) :: l)
    ht
    []

module StrMap = Map.Make(String)
type ('s,'t) strmap = 't StrMap.t constraint 's=string

let dict_to_strmap l =
  List.fold_left
    (fun m (k,v) ->
       StrMap.add k v m
    )
    StrMap.empty
    l

let strmap_to_dict m =
  StrMap.fold
    (fun k v h -> (k,v)::h)
    m
    []

type ('s,'t) complex = Complex.t constraint 's=float constraint 't=float

let pair_to_complex (re,im) = { Complex.re = re; im = im }

let complex_to_pair c = (c.Complex.re, c.Complex.im)

let pair_to_identity (n, c) : identity =
  ( object method name = n method category = c end )

let identity_to_pair (id : identity) =
  (id#name, id#category)
