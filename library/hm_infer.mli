type error =
  | Undefined of string
  | Mismatch of Hm_type.ty * Hm_type.ty
  | TypeRecursion
  | RecursiveUnification

exception Error of Hm_ast.ast * error

val infer : Hm_env.env -> Hm_ast.ast -> Hm_type.ty
  (** Infers the type of an expression in a given environment. This function
      does not modify the passed environment. *)
