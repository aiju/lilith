module H = Hashtbl
module S = Stream
open Dat

let tryoption x def = match x with Some n -> n | None -> def

let lineno = ref 1

let error s = 
	Format.eprintf "%d: %s@\n%!" !lineno s;
	exit 1

let quote s = Printf.sprintf "%S" s
type token =
	EOF | ID of Symbol.t | VAR of Symbol.t | LITERAL of string | DIRECTIVE of string | ACTION of string
	| EQUAL | LPAREN | RPAREN | COMMA | COLON | PIPE | SEMICOLON
let tokstr t = match t with
	| EOF -> "eof"
	| ID s -> Symbol.name s
	| VAR s -> "$" ^ (Symbol.name s)
	| COLON -> ":"
	| PIPE -> "|"
	| LITERAL s -> quote s
	| DIRECTIVE s -> s
	| EQUAL -> "="
	| LPAREN -> "("
	| RPAREN -> ")"
	| COMMA -> ","
	| SEMICOLON -> ";"
	| ACTION(s) -> "{" ^ s ^ "}"

let cs str c = str ^ (String.make 1 c)
let noeof fn = try fn () with Stream.Failure -> error "unexpected eof"
let rec lex s =
	match S.next s with
	| ' ' | '\t' -> lex s
	| '\n' -> lineno := !lineno + 1; lex s
	| ':' -> COLON
	| '|' -> PIPE
	| '=' -> EQUAL
	| 'a'..'z' | 'A'..'Z' | '_' | '\x80'..'\xff' as c -> ident s (cs "" c) (fun x -> ID(x))
	| '"' -> noeof(fun() -> literal s "")
	| '%' -> directive s "%"
	| '$' -> ident s "" (fun x -> VAR(x))
	| '{' -> noeof(fun() -> action s "" 0)
	| '(' -> LPAREN
	| ')' -> RPAREN
	| ',' -> COMMA
	| ';' -> SEMICOLON
	| _ -> error "unexpected character"
and ident s str res =
	match S.peek s with
	| Some ('a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '\x80'..'\xff' | '\'') -> ident s (cs str (S.next s)) res
	| _ -> res(Symbol.get str)
and literal s str =
	match S.next s with
	| '"' -> LITERAL str
	| '\n' -> error "newline in string"
	| c -> literal s (cs str c)
and directive s str =
	match S.peek s with
	| Some(' '|'\t'|'\n') -> DIRECTIVE str
	| _ -> directive s (cs str (S.next s))
and action s str n =
	let c = S.next s in match c with
	| '\n' -> lineno := !lineno + 1; action s (cs str c) n
	| '{' -> action s (cs str c) (n+1)
	| '}' -> if n == 0 then ACTION(str) else action s (cs str c) (n-1) 
	| '"' | '\'' -> action_string s (cs str c) n c
	| '(' when S.peek s = Some '*' -> S.junk s; action_comment s (str^"(*") n
	| c -> action s (cs str c) n
and action_string s str n term =
	let c = S.next s in match c with
	| '\n' -> lineno := !lineno + 1; action_string s (cs str c) n term
	| '\\' -> action_string s (cs (cs str c) (S.next s)) n term
	| x when x=term -> action s (cs str c) n
	| c -> action_string s (cs str c) n term
and action_comment s str n =
	let c = S.next s in match c with
	| '*' when S.peek s = Some ')' -> S.junk s; action s (str^"*)") n
	| '\n' -> lineno := !lineno + 1; action_comment s (cs str c) n
	| c -> action_comment s (cs str c) n
	

module Ss = Set.Make(Symbol)
type expr =
	| Sym of Symbol.t
	| ParamSym of Symbol.t * expr list
	| Var of Symbol.t
	| Literal of string
	| Seq of expr list
	| Alt of expr list
	| Prec of Symbol.t
	| Action of string

let head e = match e with
	| Sym s -> s
	| ParamSym(t,_) -> t
	| _ -> assert false

type grammar = {
	mutable terminals: Ss.t;
	mutable nonterminals: Ss.t;
	literals: (string, Symbol.t) H.t;
	prec: (Symbol.t, int * assoctype) H.t;
	mutable rules: (expr * expr) list;
	macros: (Symbol.t, expr * expr) H.t;
	mutable genrules: LALR.rule list;
	mutable start: Symbol.t
}

let newGrammar () = {
	terminals=Ss.empty;
	nonterminals=Ss.empty;
	literals=H.create 0;
	prec=H.create 0;
	rules=[];
	macros=H.create 0;
	genrules=[];
	start=Symbol.get "$"
}

let parse s g =
	let colon = ref None in
	let preclevel = ref 0 in
	let expect_id () = match S.next s with
		| ID(t) -> t
		| t -> error ("expected identifier, got "^(tokstr t))
	and expect_var () = match S.next s with
		| VAR(t) -> t
		| t -> error ("expected variable, got "^(tokstr t))
	and expect_directive () = match S.next s with
		| DIRECTIVE(t) -> t
		| t -> error ("expected directive, got "^(tokstr t))
	and expect_literal () = match S.next s with
		| LITERAL(t) -> t
		| t -> error ("expected literal, got"^(tokstr t))
	and expect tok = 
		let t = S.next s in
		if t <> tok then error ("syntax error, got "^(tokstr t)^", expected "^(tokstr tok))
	and got tok = if S.peek s = Some tok then (S.junk s; true) else false
	and peek () = match (S.peek s) with Some x -> x | None -> assert false
	in let rec p_deftokens () =
		match peek () with
		| ID(_) ->
			let t = expect_id () in
			if Ss.mem t g.terminals then
				error ("token "^(Symbol.name t)^" redefined");
			g.terminals <- Ss.add t g.terminals;
			if got EQUAL then (
				let l = expect_literal() in
				if H.mem g.literals l then
					error ("literal "^(quote l)^" redefined");
				H.add g.literals l t
			);
			p_deftokens ()
		| _ -> p_header ()
	and p_assoc assoc level =
		match peek () with
		| ID(_) ->
			let t = expect_id () in
			if not (Ss.mem t g.terminals) then
				error ("precedence declared for nonterminal");
			(match H.find_opt g.prec t with
			| Some _ -> error ("precedence of "^(Symbol.name t)^" redefined")
			| None -> H.add g.prec t (level,assoc));
			p_assoc assoc level
		| LITERAL(_) ->
			let l = expect_literal () in
			(match H.find_opt g.literals l with
			| None -> error ("undefined literal "^(quote l))
			| Some t ->
				(match H.find_opt g.prec t with
				| Some _ -> error ("precedence of "^(Symbol.name t)^" redefined")
				| None -> H.add g.prec t (level,assoc)));
			p_assoc assoc level
		| _ -> p_header ()
	and p_header () =
		match expect_directive () with
		| "%token" -> p_deftokens ()
		| "%left" -> incr preclevel; p_assoc Left (!preclevel)
		| "%right" -> incr preclevel; p_assoc Right (!preclevel)
		| "%nonassoc" -> incr preclevel; p_assoc Nonassoc (!preclevel)
		| "%%" -> ()
		| s -> error ("unknown directive "^s)
	and p_lexpr () =
		match peek () with
		| ID(_) ->
			let id = expect_id () in
			if got LPAREN then
				ParamSym(id, p_params ())
			else
				Sym(id)
		| VAR(_) -> Var(expect_var ())
		| LITERAL(_) -> Literal(expect_literal ())
		| t -> error ("syntax error, unexpected "^(tokstr t))
	and p_params () =
		if got RPAREN then
			[]
		else
			let e = p_rexpr () in
			if got COMMA then e::p_params ()
			else (expect RPAREN; [e])
	and p_rule () =
		let lhs = match !colon with
			| None -> p_lexpr ()
			| Some x -> x in
		colon := None;
		expect COLON;
		if got EQUAL then
			H.add g.macros (head lhs) (lhs, p_rexpr ())
		else
			g.rules <- (lhs, p_rexpr ()) :: g.rules;
		ignore (got SEMICOLON)
	and p_rexpr () = let e = ref (p_seq ()) in while got PIPE do e := Alt[!e; p_seq ()] done; !e
	and p_seq () =
		match p_prim () with
		| None -> Seq[]
		| Some r -> Seq[r; p_seq ()]
	and p_prim () =
		match peek() with
		| ID(_) | LITERAL(_) | VAR(_) ->
			let r = p_lexpr() in
			if peek() = COLON then (
				colon := Some(r);
				None
			) else Some(r)
		| LPAREN -> S.junk s; let r = p_rexpr () in expect RPAREN; Some(r)
		| DIRECTIVE("%prec") -> S.junk s; Some(Prec(expect_id ()))
		| ACTION(str) -> S.junk s; Some(Action(str))
		| t -> None
	and p_rules () =
		if got EOF then ()
		else (
			p_rule();
			p_rules()
		)
	in p_header ();
	p_rules ();
	g.rules <- List.rev g.rules

let rec canonize a =
	match a with
	| Alt(l) ->
		let l' = l |> List.map canonize
		|> List.map (fun x -> match x with Alt l' -> l' | _ -> [x]) |> List.concat in
		(match l' with [x] -> x | _ -> Alt l')
	| Seq(l) ->
		let l' = l |> List.map canonize
		|> List.map (fun x -> match x with Seq l' -> l' |  _ -> [x]) |> List.concat
		in (match l' with [x] -> x | _ -> Seq l')
	| Sym(_) | Var(_) | Literal(_) | Prec(_) | Action(_) -> a
	| ParamSym(t, p) -> ParamSym(t, List.map canonize p)
let rec exprMatch scope a b =
	match (a,b) with
	| Sym(t), Sym(t') -> t = t'
	| ParamSym(t,p), ParamSym(t',p') ->
		t = t' && List.length p = List.length p' && List.for_all2 (exprMatch scope) p p'
	| Var(v), _ ->
		(match H.find_opt scope v with
		| None -> H.add scope v b; true
		| Some e -> exprMatch scope b e)
	| Literal(l), Literal(l') -> l = l'
	| Alt(l), Alt(l') | Seq(l), Seq(l') -> List.length l = List.length l' && List.for_all2 (exprMatch scope) l l'
	| _ -> false
let rec exprSub g scope e =
	let macro e =
		let rec f ms =
			match ms with
			| [] -> e
			| (lhs,rhs)::t ->
				let scope' = H.create 0 in
				if exprMatch scope' (canonize lhs) (canonize e) then
					exprSub g scope' rhs
				else
					f t
		in f (List.rev (H.find_all g.macros (head e)))
	in
	match e with
	| Sym(s) -> macro e
	| Var(s) -> tryoption (H.find_opt scope s) e
	| Literal(l) ->
		(match H.find_opt g.literals l with
		| None -> error ("undefined token "^(quote l))
		| Some s -> Sym(s))
	| ParamSym(t, p) -> macro (ParamSym(t, List.map (exprSub g scope) p))
	| Alt(l) -> Alt(List.map (exprSub g scope) l)
	| Seq(l) -> Seq(List.map (exprSub g scope) l)
	| Prec _ | Action _ -> e
and exprToString e =
	let callString (t,p) = 
		let p' = List.map exprToString p in
		let q = if p' = [] then "" else List.fold_left (fun a b -> a^","^b) (List.hd p') (List.tl p') in
		t ^ "(" ^ q ^ ")" in
	match e with
	| Sym(s) -> Symbol.name s
	| Var(s) -> "$" ^ (Symbol.name s)
	| Literal(s) -> quote s
	| ParamSym(t, p) -> callString(Symbol.name t, p)
	| Seq(p) -> callString("$seq", p)
	| Alt(p) -> callString("$alt", p)
	| Prec(p) -> callString("$prec", [Sym(p)])
	| Action(a) -> assert false
let exprToSym e = Symbol.get (exprToString e)

let rec exprExpand e =
	match e with
	| Seq l ->
		let l' = List.map exprExpand l in
		let rec f left right = match right with
			| [] -> Seq (List.rev left)
			| Alt k::right' -> Alt (List.map (fun x -> Seq [f left []; x; f [] right']) k)
			| x::right' -> f (x::left) right'
		in f [] l'
	| Alt l -> Alt (List.map exprExpand l)
	| Sym _ | Var _ | Literal _ | ParamSym(_, _) | Prec _ | Action _ -> e

let rec ruleprec g rhs =
	match rhs with
	| [] -> None
	| s::t -> match H.find_opt g.prec s with
		| None -> ruleprec g t
		| Some p -> Some p

let generateRules g =
	let seen = H.create 0 in
	let idx = ref 1 in
	Ss.iter (fun x -> H.add seen (Sym x) ()) g.terminals;
	let rec f qu =
		let qu' = ref [] in
		let addRule lhs rhs prec action =
			let rhs' = List.map exprToSym rhs in
			g.genrules <- {
				lhs=exprToSym lhs;
				idx= !idx;
				rhs=rhs';
				prec=(match prec with
					| Some s -> (match H.find_opt g.prec s with
						| None -> error ("precedence for "^(Symbol.name s)^" undefined")
						| Some p -> Some p)
					| None -> ruleprec g (List.rev rhs'));
				action
			}::g.genrules;
			incr idx;
			rhs |> List.iter (fun x ->
				if not (H.mem seen x) then (
					H.replace seen x ();
					qu' := x::!qu'
				)
			)
		in let rec processRule prefix lhs rhs =
			match rhs with
			| Alt(l) -> List.iter (processRule prefix lhs) l
			| Seq(l) ->
				let prec = ref None in
				let action = ref None in
				let rec proc_rhs rhs = match rhs with
					| [] -> []
					| ((Sym _ | ParamSym(_, _)) as h)::t -> h::proc_rhs t
					| [Action(a)] -> action := Some(a); []
					| Action(a)::t ->
						let s' = Sym (Symbol.temp prefix) in
						processRule prefix s' (Action(a));
						H.add seen s' ();
						s'::proc_rhs t
					| Prec(p)::t ->
						if !prec = None then (
							prec := Some p;
							proc_rhs t
						) else
							error ("precedence of rule "^(exprToString lhs)^" redefined")
					| (Var _ | Literal _ | Seq _ | Alt _)::_ -> assert false
				in let rhs' = proc_rhs l in
				addRule lhs rhs' !prec !action
			| _ -> processRule prefix lhs (Seq([rhs]))
		in qu |> List.iter (fun p ->
			let found = ref false in
			g.rules |> List.iter (fun (lhs,rhs) ->
				let scope = H.create 0 in
				if exprMatch scope (canonize lhs) (canonize p) then (
					found := true;
					let lhs' = canonize (exprSub g scope lhs) and
					rhs' = canonize (exprExpand (exprSub g scope rhs)) in
					processRule (exprToString lhs' ^ "$") lhs' rhs'
				)
			);
			if not !found then 
				error ("undefined "^(exprToString p));
		);
		if !qu' <> [] then f !qu'
	in match (List.hd g.rules) with (lhs,_) ->
		(match lhs with
		| Sym(s) -> g.start <- s
		| _ -> error "invalid start rule");
		H.replace seen lhs ();
		f [lhs]

let _ =
	(if Array.length Sys.argv < 2 then (Printf.eprintf "usage: %s file\n%!" Sys.argv.(0); exit 1));
	let f = S.of_channel (open_in Sys.argv.(1)) in
	let toks = S.from (fun _ -> Some (try lex f with Stream.Failure -> EOF)) in
	let g = newGrammar () in
	parse toks g;
	generateRules g;
	let lalr = LALR.create g.genrules g.prec g.start in
	LALR.printStates Format.std_formatter lalr



