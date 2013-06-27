type var = int

type ty =
  | Var of (*name*)var
  | Oper of (*name*)string * (*types*)ty list


let oper name types =
  Oper (name, types)


let new_fun arg result =
  oper "->" [arg; result]


let zero_var = 0
let succ_var var = var + 1


let rec string_of_var var =
  let a = int_of_char 'a' in
  if var < 26 then
    BatString.of_char (char_of_int (int_of_char 'a' + var))
  else
    string_of_var (var / 26 - 1) ^ string_of_var (var mod 26)


let var_of_string s =
  let a = int_of_char 'a' in
  snd (BatString.fold_right (fun c (multiplier, var) ->
    multiplier * 26, var + (int_of_char c - a + 1) * multiplier
  ) s (1, 0)) - 1


let rec string_of_type = function
  | Var name ->
      string_of_var name

  (* print binary operators as "arg1 op arg2" *)
  | Oper (opname, [arg1; arg2]) ->
      "(" ^
      (string_of_type arg1) ^
      " " ^
      opname ^
      " " ^
      (string_of_type arg2) ^
      ")"

  (* other operators are printed as "op arg1 arg2 ..." *)
  | Oper (opname, args) ->
      List.fold_left (fun args arg ->
        args ^ " " ^ (string_of_type arg)
      ) opname args


let print ty = print_string (string_of_type ty)
