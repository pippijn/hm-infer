type var

type ty =
  | Var of var
  | Oper of string * ty list

val oper : string -> ty list -> ty
val new_fun : ty -> ty -> ty

val zero_var : var
val succ_var : var -> var

val string_of_var : var -> string
val var_of_string : string -> var

val string_of_type : ty -> string

val print : ty -> unit
