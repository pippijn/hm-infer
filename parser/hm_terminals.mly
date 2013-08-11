%token EOF			"end of input"

%token KW_LET			"let"
%token KW_REC			"rec"
%token KW_FUN			"fun"
%token KW_IN			"in"

%token TK_EQUALS		"="
%token TK_STAR			"*"
%token TK_ARROW			"->"
%token TK_LBRACK		"("
%token TK_RBRACK		")"

%token<string> TK_IDENTIFIER	"identifier"
%token<string> TK_TVAR		"type variable"
%token<int> TK_INTEGER		"integer"

%%
