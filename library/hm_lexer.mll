{
  open Hm_parser
}


let num = ['0'-'9']+
let c = ['a'-'z']
let d = ['0'-'9']
let id = c(c|d)*


rule token = parse
  | "let"		{ KW_LET }
  | "rec"		{ KW_REC }
  | "fun"		{ KW_FUN }
  | "in"		{ KW_IN }

  | "*"			{ TK_STAR }
  | "="			{ TK_EQUALS }
  | "->"		{ TK_ARROW }
  | "("			{ TK_LBRACK }
  | ")"			{ TK_RBRACK }

  | id as id		{ TK_IDENTIFIER id }
  | '\'' (c+ as tvar)	{ TK_TVAR tvar }
  | num as num		{ TK_INTEGER (int_of_string num) }

  | [' ' '\t' '\n']	{ token lexbuf }

  | eof			{ EOF }
