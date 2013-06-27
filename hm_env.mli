(** The type environment. This data structure is purely functional. Fresh
    copies are created on each modification. *)
type env

val empty : env
  (** Create a new environment with the first type variable set to 'a'. *)
val combine : env -> env -> env
  (** Return the first [env] but with the second [env]'s type variable
      instances and variable count. *)

val add : string -> Hm_type.ty -> env -> env
  (** Returns a new environment with a typed symbol added to it. Leaves the
      old environment untouched. *)
val find : string -> env -> Hm_type.ty
  (** Return the type added by [add].
      @raise [Not_found] if there was no corresponding call to [add]. *)
val mem : string -> env -> bool
  (** Return [true] if [find] would not raise [Not_found]. *)
val iter : (string * Hm_type.ty -> unit) -> env -> unit
  (** Iterate over symbol/type pairs. *)
val fold_left : ('a -> string * Hm_type.ty -> 'a) -> 'a -> env -> 'a
  (** Fold over symbol/type pairs. *)

val add_instance : Hm_type.var -> Hm_type.ty -> env -> env
  (** Returns a new environment with an instance attached to a type variable.
      Leaves the old environment untouched. *)
val find_instance : Hm_type.var -> env -> Hm_type.ty
  (** Return the instance added by [add_instance].
      @raise [Not_found] if there was no corresponding call to [add_instance]. *)
val mem_instance : Hm_type.var -> env -> bool
  (** Return [true] if [find_instance] would not raise [Not_found]. *)
val iter_instance : (Hm_type.var * Hm_type.ty -> unit) -> env -> unit
  (** Iterate over var/instance pairs. *)

val new_var : env -> (env * Hm_type.ty)
  (** Create a new type variable unique for this environment and return a new
      environment that will generate a different type variable. *)
