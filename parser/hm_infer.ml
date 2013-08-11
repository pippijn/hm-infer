open Hm_type

type error =
  | Undefined of string
  | Mismatch of Hm_type.ty * Hm_type.ty
  | TypeRecursion
  | RecursiveUnification

exception Error of Hm_ast.ast * error
exception Type_error of error


let die error = raise (Type_error error)


(** Generate a fresh (renamed) variable
    keeps an environment of variables that
    have already been renamed, so that
    (t->t) gets renamed to (q->q), not (q->r). *)
let freshvar (env, renamed) ty =
  try 
    (env, renamed), snd (List.find (fun (a, _) -> a = ty) renamed)
  with Not_found ->
    let env, newtype = Hm_env.new_var env in
    let renamed = (ty, newtype) :: renamed in
    (env, renamed), newtype


(** Resolves all specialised type variables to their instances.
    Renames generic types starting with 'a'. *)
let resolve env ty =
  let rec resolve_vars fresh var_env = function
    | Var name as ty ->
        begin try
          resolve_vars fresh var_env (Hm_env.find_instance name env)
        with Not_found ->
          fresh var_env ty
        end
    | Oper (name, args) ->
        let var_env, args =
          List.fold_left (fun (var_env, args) arg ->
            let var_env, arg = resolve_vars fresh var_env arg in
            var_env, arg :: args
          ) (var_env, []) args
        in
        var_env, Oper (name, List.rev args)
  in
  snd (resolve_vars (fun vars ty -> freshvar vars ty) (Hm_env.empty, []) ty)


(** Returns the currently defining instance of [ty]. *)
let rec prune env = function
  | Var name as ty ->
      begin try
        let instance = Hm_env.find_instance name env in
        if ty == instance then
          die TypeRecursion;
        prune env instance
      with Not_found ->
        ty
      end
  | ty -> ty


(** Figure out if variable [type1] occurs anywhere in type [type2]. *)
let rec occursintype type1 type2 =
  match type2 with
  | Var _ ->
      type1 == type2
  | Oper (_, args) ->
      List.exists (occursintype type1) args


(** Generate a fresh type, copying the generic variables. *)
let fresh env ty nongen =
  let rec fresh vars ty =
    match prune env ty with
    | Var _ as ty ->
        if List.exists (occursintype ty) nongen then
          (* if non-generic *)
          vars, ty
        else
          freshvar vars ty

    | Oper (name, args) ->
        (* recursively operate on the arguments *)
        let vars, args =
          List.fold_left (fun (vars, args) arg ->
            let vars, arg = fresh vars arg in
            vars, arg :: args
          ) (vars, []) args
        in
        vars, Oper (name, List.rev args)
  in
  (* maintains an environment of variables that
   * have already been copied/renamed *)
  let (env, _), ty = fresh (env, []) ty in
  env, ty



(** Retrieves the type of an ident from the environment,
    generating a fresh copy of the generic variables. *)
let gettype name env nongen =
  let ty =
    try
      Hm_env.find name env
    with Not_found ->
      die (Undefined name)
  in
  fresh env ty nongen


let rec unify env ty1 ty2 nongen =
  (* reduce to types we currently care about *)
  let ty1 = prune env ty1 in
  let ty2 = prune env ty2 in

  match ty1, ty2 with
  (* do NOT unify a non-generic variable with a generic one:
   * we have to "taint" the generic variable during unification
   * so that we no longer get fresh copies of it *)
  | Var _, Var _
      when List.exists (occursintype ty1) nongen
      && not (List.exists (occursintype ty2) nongen) ->
        let env, ty2, ty1 = unify env ty2 ty1 nongen in
        env, ty1, ty2

  | Var _, _
      when occursintype ty1 ty2 && ty1 <> ty2 ->
        die RecursiveUnification

  | Var name, ty2 ->
      (* define a type instance *)
      Hm_env.add_instance name ty2 env, ty1, ty2

  | Oper (name, args), Var _ ->
      let env, ty2, ty1 = unify env ty2 ty1 nongen in
      env, ty1, ty2

  | Oper (name1, args1), Oper (name2, args2) when name1 = name2 ->
      if List.length args1 <> List.length args2 then
        die (Mismatch (resolve env ty1, resolve env ty2));

      (* unify arguments *)
      let env, args1, args2 =
        List.fold_left2 (fun (env, args1, args2) arg1 arg2 ->
          let env, arg1, arg2 = unify env arg1 arg2 nongen in
          env, arg1 :: args1, arg2 :: args2
        ) (env, [], []) args1 args2
      in
      env, Oper (name1, List.rev args1), Oper (name2, List.rev args2)

  | Oper _, Oper _ ->
      die (Mismatch (resolve env ty1, resolve env ty2))


(** Recursively infer the type of an [ast] expression. *)
let rec analyse env nongen ast =
  let open Hm_ast in

  try
    match ast with
    (* An already-typed literal. *)
    | Typed (_, ty) ->
        env, ty

    (* Identifiers. *)
    | Ident (name) ->
        gettype name env nongen

    (* Function application. *)
    | FunApp (fn, arg) ->
        let oldenv = env in

        let env, funtype = analyse env nongen fn in
        let env, argtype = analyse env nongen arg in

        (* unify (argtype -> alpha) funtype *)
        let env, restype =
          let env, restype = Hm_env.new_var env in
          match unify env (Oper ("->", [argtype; restype])) funtype nongen with
          | env, Oper ("->", [_; restype]), _ -> env, restype
          | _ -> failwith "impossible"
        in

        (* Keep type variable instances and last variable name from then new env,
         * throw away the identifiers. *)
        (Hm_env.combine oldenv env), restype

    (* Lambda expression. *)
    | Lambda (var, body) ->
        let oldenv = env in

        (* create a new non-generic variable for the binder *)
        let env, argtype =
          match Hm_env.new_var env with
          | env, (Var _ as argtype) ->
              Hm_env.add var argtype env, argtype
          | _ -> failwith "impossible"
        in

        (* analyse the expression with this new variable *)
        let env, restype = analyse env (argtype :: nongen) body in

        (* create the appropriate return type *)
        (Hm_env.combine oldenv env), Hm_type.new_fun argtype restype

    (* "let". *)
    | Let (var, defn, body) ->
        let oldenv = env in

        (* analyse the definition *)
        let env, defntype = analyse env nongen defn in
        (* this is then the type of the binder (which is now generic) *)
        let env = Hm_env.add var defntype env in
        (* analyse the body *)
        let env, lettype = analyse env nongen body in

        (* Keep type variable instances. *)
        (Hm_env.combine oldenv env), lettype

    (* "let rec". *)
    | LetRec (var, defn, body) ->
        let oldenv = env in

        (* generate a new non-generic type for the binder *)
        let env, newtype = Hm_env.new_var env in
        let env = Hm_env.add var newtype env in
        (* analyse the type of the definition, with the binder type
         * being non-generic (as if we were using the fixed point combinator) *)
        let env, defntype = analyse env (newtype :: nongen) defn in
        (* unify to obtain the proper type of binder *)
        let env, _, _ = unify env newtype defntype nongen in
        (* which now becomes generic while analysing the body *)
        let env, lettype = analyse env nongen body in

        (* Keep type variable instances. *)
        (Hm_env.combine oldenv env), lettype
  with
  | Type_error e ->
      raise (Error (ast, e))


let infer env ast =
  let env, ty = analyse env [] ast in
  resolve env ty
