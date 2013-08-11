open Hm_tokens

let string_of_token = function
  | TK_TVAR tvar -> "'" ^ tvar
  | TK_INTEGER d -> string_of_int d
  | TK_IDENTIFIER id -> id
  | TK_STAR -> "*"
  | TK_LBRACK -> "("
  | TK_RBRACK -> ")"
  | TK_EQUALS -> "="
  | TK_ARROW -> "->"
  | KW_REC -> "rec"
  | KW_LET -> "let"
  | KW_IN -> "in"
  | KW_FUN -> "fun"
  | EOF -> "<eof>"
