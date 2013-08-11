let (|>) x f = f x

let expr_of_string code =
  let lexbuf = Lexing.from_string code in
  Hm_parser.expression Hm_lexer.token lexbuf


let type_of_string code =
  let lexbuf = Lexing.from_string code in
  Hm_parser.type_expression Hm_lexer.token lexbuf


let env =
  Hm_env.empty

  |> Hm_env.add "true" (type_of_string "bool")
  |> Hm_env.add "false" (type_of_string "bool")

  (* pair: a -> b -> (a * b) *)
  |> Hm_env.add "pair" (type_of_string "'a -> 'b -> 'a * 'b")

  (* cond: bool -> a -> a -> a *)
  |> Hm_env.add "cond" (type_of_string "bool -> 'c -> 'c -> 'c")

  (* zero: int -> bool *)
  |> Hm_env.add "zero" (type_of_string "int -> bool")

  (* pred: int -> int *)
  |> Hm_env.add "pred" (type_of_string "int -> int")

  (* times: int -> int -> int *)
  |> Hm_env.add "times" (type_of_string "int -> int -> int")


let tryexp exp =
  Hm_ast.print exp;
  print_string " : ";
  begin try
    let ty = Hm_infer.infer env exp in
    Hm_type.print ty
  with Hm_infer.Error (ast, e) ->
    begin match e with
    | Hm_infer.TypeRecursion ->
        print_string "type recursion"
    | Hm_infer.RecursiveUnification ->
        print_string "recursive unification"
    | Hm_infer.Undefined symbol ->
        print_string "undefined symbol: ";
        print_string symbol
    | Hm_infer.Mismatch (ty1, ty2) ->
        print_string "type mismatch: ";
        Hm_type.print ty1;
        print_string " <> ";
        Hm_type.print ty2
    end;
    print_newline ();
    print_string (Hm_ast.caret_at ast exp)
  end;
  print_newline ()


let infer code =
  tryexp (expr_of_string code)


let selftest () =
  Printexc.record_backtrace true;

  print_string "-- environment --\n";
  Hm_env.iter (fun (name, ty) ->
    Printf.printf "%s: " name;
    Hm_type.print ty;
    print_newline ()
  ) env;

  print_string "-- expressions --\n";
  infer "1";
  infer "1 1";
  infer "true";
  infer "zero 0";
  infer "zero true";
  infer "pair 2 2";
  infer "pair";
  infer "fun f -> fun g -> pair f g";
  infer "fun f -> f f";
  infer "let g = 0 in g";
  infer "let g = fun f -> 5 in g g";
  infer "fun g -> let f = fun x -> g in pair (f 3) (f true)";
  infer "fun f -> pair f";
  infer "let f = fun x -> x in pair (f 3) (f true)";
  infer "let factorial = fun n -> cond (zero n) 1 (times n (factorial (pred n))) in factorial 5";
  infer "let rec factorial = fun n -> cond (zero n) 1 (times n (factorial (pred n))) in factorial 5";

  ()


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; "-"|] ->
      let lexbuf = Lexing.from_channel stdin in
      begin try
        tryexp (Hm_parser.expression Hm_lexer.token lexbuf)
      with Hm_parser.StateError (token, state) ->
        Printf.printf "%s\n" (Hm_errors.message state token)
      end
  | [|_; "-merr"|] ->
      let lexbuf = Lexing.from_channel stdin in
      begin try
        tryexp (Hm_parser.expression Hm_lexer.token lexbuf)
      with Hm_parser.StateError (token, state) ->
        Printf.printf "(%d, TOKEN)\n" state
      end
  | _ ->
      selftest ()
