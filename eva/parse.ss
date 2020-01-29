%{
	open Ast
	
	let p = posed
%}
%token
	TCOLON=":"
	TEQUAL="="
	TEQEQ="=="
	TNE="!="
	TLT="<"
	TLE="<="
	TGT=">"
	TGE=">="
	TSEMICOLON=";"
	TPLUS="+"
	TMINUS="-"
	TMUL="*"
	TDIV="/"
	TMOD="%"
	TLPAREN="("
	TRPAREN=")"
	TLBRACKET="["
	TRBRACKET="]"
	TARROW="->"
	TCOMMA=","
	TIF="if"
	TELSE="else"
	TWHILE="while"
	TDO="do"
	TFOR="for"
	TVAR="var"
	UMINUS

%token <Symbol.t> TSYM
%token <int> TINTLIT

%type <unit Ast.ast> program

%unary "if"
%unary "else"
%right "->"
%nonassoc "==" "!=" "<" "<=" ">" ">="
%right "=" ":"
%left "+" "-"
%left "*" "/" "%"
%nonassoc UMINUS

%%

program: sequence(optstat ";") { p _pos (Fix($1)) }

sequence($X): { [] } | sequence($X) $X { $1 @ [$2] }
optional($X):= { None } | $X { Some $1 }
list($X): { [] } | list1($X) (| ",")
list1($X): $X { [$1] } | list1($X) "," $X { $1 @ [$2] }

optstat: { p _pos (Seq[]) } | stat(c)

stat($X):
	expr($X)
	| expr($X) "=" expr($X) { p _pos (Assign($1, $2)) }
	| "if" "(" stat($X) ")" stat($X) {p _pos (If($1,$2,p _pos (Seq[])))}
	| "if" "(" stat($X) ")" stat($X) "else" stat($X) {p _pos (If($1,$2,$3))}
	| "while" "(" stat($X) ")" stat($X) {p _pos (While($1,$2))}
	| "do" stat($X) "while" "(" stat($X) ")" {p _pos (DoWhile($1,$2))}

expr(c) := cexpr
expr(n) := nexpr

cexpr:
	nexpr
	| nexpr "," list(nexpr) { p _pos (Tuple($1::$2)) }
	| cexpr "->" cexpr { p _pos (Lambda($1, $2)) }

nexpr:
	primary
	| nexpr ":" optional(nexpr) { p _pos (TypeIs($1,$2)) }
	| nexpr binop nexpr { p _pos (Bin($2, $1, $3))}
	| "-" nexpr %prec UMINUS { p _pos (Un(OpUMinus, $1)) }
	| TVAR nexpr %prec UMINUS { p _pos (Un(OpVar, $1)) }

binop :=
	"+" { OpAdd }
	| "-" { OpSub }
	| "*" { OpMul }
	| "/" { OpDiv }
	| "%" { OpMod }
	| "==" { OpEq }
	| "!=" { OpNe }
	| "<" { OpLt }
	| ">" { OpGt }
	| "<=" { OpLe }
	| ">=" { OpGe }
	
primary:
	TSYM { p _pos (Sym($1)) }
	| TINTLIT { p _pos (IntLit($1)) }
	| primary "(" list(stat(n)) ")" { p _pos (Call($1, $2)) }
	| primary "[" list(stat(n)) "]" { p _pos (Index($1, $2)) }
	| "(" stat(c) ")"
	| "(" optstat ";" sequence(optstat ";") optstat ")" { p _pos (Seq ([$1] @ $2 @ [$3])) }
	| "[" list(stat(n)) "]" { p _pos (Array $1) }
