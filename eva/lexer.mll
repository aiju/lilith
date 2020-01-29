{
	open Parse_gen
	exception LexErr of int * int
	let pos b = (Lexing.lexeme_start b, Lexing.lexeme_start b)
	let lex_error lexbuf =
		let l = (Lexing.lexeme_end_p lexbuf) in
			raise (LexErr (l.pos_lnum, l.pos_cnum - l.pos_bol))
	let kwtab = [
		"if", TIF;
		"else", TELSE;
		"while", TWHILE;
		"do", TDO;
		"for", TFOR;
		"var", TVAR
	]
	let kwfind s =
		let rec f t = match t with
		| (a,b)::_ when a=s -> b
		| _::t -> f t
		| [] -> TSYM(Symbol.get s)
		in f kwtab
}

rule token = parse
	  [' ' '\t']
	  	{ token lexbuf }
	| '\n'	{ Lexing.new_line lexbuf ; token lexbuf }
	| "/*"	{ comment lexbuf }
	| (['A'-'Z' 'a'-'z' '\x80'-'\xff'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\x80'-'\xff']*) as s { kwfind s }
	| ['0'-'9']+ as s { TINTLIT(int_of_string(s)) }
	| ':' { TCOLON } 
	| "==" { TEQEQ }
	| "!=" { TNE }
	| "<=" { TLE }
	| '<' { TLT }
	| ">=" { TGE }
	| '>' { TGT }
	| "->" { TARROW }
	| '=' { TEQUAL } 
	| ';' { TSEMICOLON } 
	| '+' { TPLUS } 
	| '-' { TMINUS } 
	| '*' { TMUL } 
	| '/' { TDIV } 
	| '%' { TMOD } 
	| '(' { TLPAREN } 
	| ')' { TRPAREN } 
	| '[' { TLBRACKET } 
	| ']' { TRBRACKET } 
	| ',' { TCOMMA } 
	| eof	{ TEOF }
	| _	{ lex_error lexbuf }
and comment = parse
	  "*/"	{ token lexbuf }
	| '\n'	{ Lexing.new_line lexbuf ; comment lexbuf }
	| eof	{ lex_error lexbuf }
	| _	{ comment lexbuf }
and string = parse
	  "\\\"" { "\"" ^ (string lexbuf) }
	| "\\n" { "\n" ^ (string lexbuf) }
	| "\\t" { "\t" ^ (string lexbuf) }
	| "\\r" { "\r" ^ (string lexbuf) }
	| "\\\\" { "\\" ^ (string lexbuf) }
	| '"' { "" }
	| '\n'	{ lex_error lexbuf }
	| eof	{ lex_error lexbuf }
	| _ as c	{ (String.make 1 c) ^ (string lexbuf) }
