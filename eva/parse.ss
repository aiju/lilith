%{
	open Ast
	
	let p = posed
	
	let deseq x =
		match x.p with
		| Seq l -> l
		| _ -> [x]
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
	TLET="let"
	TIN="in"
	TFIX="fix"
	UMINUS

%token <Symbol.t> TSYM
%token <int> TINTLIT

%type <unit Ast.ast> program

%unary "if" "let"
%unary "else" "in"
%right ":"
%nonassoc "==" "!=" "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc UMINUS

%%

program: sequence(stat(c,s)) { p _pos (Fix($1)) }

sequence($X): { [] } | sequence($X) $X { $1 @ [$2] }
optional($X):= { None } | $X { Some $1 }
list($X): { [] } | list1($X) (| ",")
list1($X): $X { [$1] } | list1($X) "," $X { $1 @ [$2] }

eseq($x):
	stat($x,n)
	| seq0($x) empty { p _pos (Seq ($1@[$2])) }
	| seq0($x) stat($x,n) { p _pos (Seq ($1@[$2])) }
seq0($x): stat($x,s) { [$1] } | seq0($x) stat($x,s) { $1 @ [$2] }
empty: { p _pos (Seq[]) }

stat($x,s):
	";" { p _pos (Seq[]) }
	| expr($x) ";"
	| expr($x) "=" stat($x,n) ";" { p _pos (Assign($1, $2)) }
	| expr($x) "->" stat($x,n) ";" { p _pos (Lambda($1, $2)) }

stat($x,n):
	expr($x)
	| expr($x) "=" stat($x,n) { p _pos (Assign($1, $2)) }
	| expr($x) "->" stat($x,n) { p _pos (Lambda($1, $2)) }

stat($x,$y):
	 "if" "(" eseq(c) ")" stat($x,$y) {p _pos (If($1,$2,p _pos (Seq[])))}
	| "if" "(" eseq(c) ")" stat($x,$y) "else" stat($x,$y) {p _pos (If($1,$2,$3))}
	| "while" "(" eseq(c) ")" stat($x,$y) {p _pos (While($1,$2))}
	| "do" stat($x,$y) "while" "(" eseq(c) ")" {p _pos (DoWhile($1,$2))}
	| "for" "(" stat(c,n) ";" stat(c, n) ";" stat(c,n) ")" stat($x, $y) { p _pos (For($1,$2,$3,$4)) }
	| "let" stat($x,$y) {p _pos (Let($1,None))}
	| "let" stat($x,$y) "in" stat($x,$y) {p _pos (Let($1, Some $2))}
	| "fix" stat($x,$y) { p _pos (Fix(deseq $1)) }

expr(c) := cexpr
expr(n) := nexpr

cexpr:
	nexpr
	| nexpr "," list(nexpr) { p _pos (Tuple($1::$2)) }

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
	| primary "(" list(eseq(n)) ")" { p _pos (Call($1, $2)) }
	| primary "[" list(eseq(n)) "]" { p _pos (Index($1, $2)) }
	| "(" eseq(c) ")"
	| "[" list(eseq(n)) "]" { p _pos (Array $1) }
