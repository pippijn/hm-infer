%{
  open Hm_ast
%}

%right TK_ARROW
%right TK_STAR

%start expression type_expression
%type<Hm_ast.ast> expression
%type<Hm_type.ty> type_expression

%%
expression
	: functional_expression EOF
		{ $1 }
        ;


functional_expression
	: functional_expression atomic_expression
		{ FunApp ($1, $2) }
	| atomic_expression
		{ $1 }
	| KW_LET TK_IDENTIFIER TK_EQUALS functional_expression KW_IN functional_expression
		{ Let ($2, $4, $6) }
	| KW_LET KW_REC TK_IDENTIFIER TK_EQUALS functional_expression KW_IN functional_expression
		{ LetRec ($3, $5, $7) }
	| KW_FUN TK_IDENTIFIER TK_ARROW functional_expression
		{ Lambda ($2, $4) }
	;


atomic_expression
	: TK_LBRACK functional_expression TK_RBRACK
		{ $2 }
	| TK_IDENTIFIER
		{ Ident $1 }
	| TK_INTEGER
		{ Typed (string_of_int $1, Hm_type.oper "int" []) }
	;


type_expression
	: prefix_type_expression EOF
		{ $1 }
        ;


prefix_type_expression
	: TK_TVAR
        	{ Hm_type.Var (Hm_type.var_of_string $1) }
	| TK_IDENTIFIER
        	{ Hm_type.Oper ($1, []) }
	| prefix_type_expression TK_ARROW prefix_type_expression
        	{ Hm_type.Oper ("->", [$1; $3]) }
	| prefix_type_expression TK_STAR prefix_type_expression
        	{ Hm_type.Oper ("*", [$1; $3]) }
	| TK_LBRACK prefix_type_expression TK_RBRACK
        	{ $2 }
        ;
