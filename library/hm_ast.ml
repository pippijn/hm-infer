type ast =
  | Lambda of (*var*)string * (*body*)ast
  | Ident of (*name*)string
  | Typed of (*value*)string * Hm_type.ty
  | FunApp of (*fn*)ast * (*arg*)ast
  | Let of (*var*)string * (*defn*)ast * (*body*)ast
  | LetRec of (*var*)string * (*defn*)ast * (*body*)ast


let rec string_of_ast = function
  | Ident name ->
      name
  | Typed (value, _) ->
      value
  | ast ->
      "(" ^
      begin match ast with
      | Lambda (var, body) ->
          Printf.sprintf "fun %s -> " var ^
          string_of_ast body
      | FunApp (fn, arg) ->
          string_of_ast fn ^
          " " ^
          string_of_ast arg
      | Let (var, defn, body) ->
          Printf.sprintf "let %s = " var ^
          string_of_ast defn ^
          " in " ^
          string_of_ast body
      | LetRec (var, defn, body) ->
          Printf.sprintf "let rec %s = " var ^
          string_of_ast defn ^
          " in " ^
          string_of_ast body
      | Typed _
      | Ident _ -> failwith "impossible"
      end ^
      ")"


let print exp = print_string (string_of_ast exp)

let caret_at at exp =
  let buf = Buffer.create 50 in

  let rec print column exp =
    if exp == at then
      let rec dots = function
        | 0 -> ()
        | n -> Buffer.add_char buf '.'; dots (n - 1)
      in
      dots column;
      Buffer.add_char buf '^';
      column
    else
      match exp with
      | Ident name ->
          column + String.length name
      | Typed (value, _) ->
          column + String.length value
      | ast ->
          let column = column + 1 in
          let column =
            match ast with
            | Lambda (var, body) ->
                let column = column + 4 + (String.length var) + 4 in
                print column body
            | FunApp (var, body) ->
                print (print column var + 1) body
            | Let (var, defn, body) ->
                let column = column + 4 + (String.length var) + 3 in
                print (print column defn + 4) body
            | LetRec (var, defn, body) ->
                let column = column + 8 + (String.length var) + 3 in
                print (print column defn + 4) body
            | _ -> failwith "impossible"
          in
          column + 1
  in
  ignore (print 0 exp);
  Buffer.contents buf
