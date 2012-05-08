let title = "Hindley-Milner example"
let missing_title = "Environment"
let code_title = "Expression"
let sexp_title = "Inferred type"
let output_title = "Deparsed"

let example = "let f = fun x -> x in pair (f 3) (f true)"

let lang_mime_type = "text/x-ocaml"


let expr_of_string code =
  let lexbuf = Lexing.from_string code in
  Hm_parser.expression Hm_lexer.token lexbuf


let type_of_string code =
  let lexbuf = Lexing.from_string code in
  Hm_parser.type_expression Hm_lexer.token lexbuf


let env =
  let env = Hm_env.create () in

  let env = Hm_env.add env "true" (type_of_string "bool") in
  let env = Hm_env.add env "false" (type_of_string "bool") in

  (* pair: a -> b -> (a * b) *)
  let env = Hm_env.add env "pair" (type_of_string "'a -> 'b -> 'a * 'b") in

  (* cond: bool -> a -> a -> a *)
  let env = Hm_env.add env "cond" (type_of_string "bool -> 'c -> 'c -> 'c") in

  (* zero: int -> bool *)
  let env = Hm_env.add env "zero" (type_of_string "int -> bool") in

  (* pred: int -> int *)
  let env = Hm_env.add env "pred" (type_of_string "int -> int") in

  (* times: int -> int -> int *)
  let env = Hm_env.add env "times" (type_of_string "int -> int -> int") in

  env


let parse wcols code sexp output error missing =
  let env_list =
    Hm_env.fold_left (fun l (name, ty) ->
      Printf.sprintf "%s: %s" name (Hm_type.string_of_type ty) :: l
    ) [] env
  in

  missing env_list;

  try
    let expr = expr_of_string code in
    output (Hm_ast.string_of_ast expr);

    begin try
      let ty = Hm_infer.infer env expr in
      sexp (Hm_type.string_of_type ty)
    with Hm_infer.Error (ast, e) ->
      let msg =
        match e with
        | Hm_infer.TypeRecursion ->
            "type recursion"
        | Hm_infer.RecursiveUnification ->
            "recursive unification"
        | Hm_infer.Undefined symbol ->
            "undefined symbol: " ^ symbol
        | Hm_infer.Mismatch (ty1, ty2) ->
            "type mismatch: " ^
            Hm_type.string_of_type ty1 ^
            " <> " ^
            Hm_type.string_of_type ty2
      in
      let fullmsg =
        "(* error: " ^ msg ^ " *)\n" ^
        Hm_ast.string_of_ast expr ^ "\n" ^
        Hm_ast.caret_at ast expr
      in
      error ("error: " ^ msg);
      output fullmsg
    end
  with
  | Hm_parser.StateError (token, state) ->
      error (Hm_errors.message state token)
  | Failure msg ->
      error msg
