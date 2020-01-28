module H = Hashtbl
module S = Stream
open Dat

let tryoption x def = match x with Some n -> n | None -> def

let ifname = ref ""
let lineno = ref 1
let footer = ref ""

let error s = 
	Format.eprintf "%d: %s@\n%!" !lineno s;
	exit 1

let quote s = Printf.sprintf "%S" s

let cs str c = str ^ (String.make 1 c)
let rec fragstr fs =
	match fs with
	| [] -> ""
	| Piece(s)::t -> s^(fragstr t)
	| Var(i)::t -> (Format.sprintf "$%d" i)^(fragstr t)
	| Line(_,_)::t -> fragstr t
let fragchar fs c = match fs with
	| Piece s::t -> Piece (cs s c)::t
	| _ -> (Piece (String.make 1 c))::fs

type token =
	EOF | ID of Symbol.t | VAR of Symbol.t | LITERAL of string | DIRECTIVE of string | CODE of fragment list | CODEBLOCK of string
	| TYPE of string
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
	| CODE(s) -> "{" ^ (fragstr s) ^ "}"
	| TYPE(s) -> "<" ^ s ^ ">"
	| CODEBLOCK(s) -> "%{" ^ s ^ "%}"

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
	| '%' when S.peek s = Some '{' -> (S.junk s; codeblock s "") 
	| '%' -> directive s "%"
	| '$' -> ident s "" (fun x -> VAR(x))
	| '{' -> noeof(fun() -> CODE(List.rev (code s [Line (!ifname, !lineno)])))
	| '<' -> noeof(fun() -> l_type s "")
	| '(' -> LPAREN
	| ')' -> RPAREN
	| ',' -> COMMA
	| ';' -> SEMICOLON
	| '/' when S.peek s = Some '/' -> (S.junk s; comment s)
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
and l_type s str =
	match S.next s with
	| '>' -> TYPE str
	| '\n' -> error "newline in type"
	| c -> l_type s (cs str c)
and directive s str =
	match S.peek s with
	| Some(' '|'\t'|'\n') -> DIRECTIVE str
	| _ -> directive s (cs str (S.next s))
and code s frags =
	let c = S.next s in match c with
	| '\n' -> lineno := !lineno + 1; code s (fragchar frags c)
	| '{' -> let frags' = code s (fragchar frags '{') in code s (fragchar frags' '}')
	| '}' -> frags
	| '"' | '\'' -> let str' = code_string s (String.make 1 c) c in code s (Piece str' :: frags)
	| '(' when S.peek s = Some '*' ->
		S.junk s;
		let str' = code_comment s "(*"
		in code s (Piece str' :: frags)
	| '$' -> code s (code_var s "" :: frags)
	| c -> code s (fragchar frags c)
and code_var s str =
	match S.peek s with
	| Some('0'..'9') -> code_var s (cs str (S.next s))
	| _ -> Var(int_of_string str)
and code_string s str term =
	let c = S.next s in match c with
	| '\n' -> lineno := !lineno + 1; code_string s (cs str c) term
	| '\\' -> code_string s (cs (cs str c) (S.next s)) term
	| x when x=term -> cs str c
	| c -> code_string s (cs str c) term
and code_comment s str =
	let c = S.next s in match c with
	| '*' when S.peek s = Some ')' -> S.junk s; str^"*)"
	| '\n' -> lineno := !lineno + 1; code_comment s (cs str c)
	| c -> code_comment s (cs str c)
and codeblock s str =
	let c = S.next s in match c with
	| '\n' -> lineno := !lineno + 1; codeblock s (cs str c)
	| '%' when S.peek s = Some '}' -> S.junk s; CODEBLOCK(str)
	| '"' | '\'' -> let str' = code_string s (cs str c) c in codeblock s str'
	| '(' when S.peek s = Some '*' -> S.junk s; let str' = code_comment s (str^"(*") in codeblock s str'
	| c -> codeblock s (cs str c)
and comment s =
	match S.peek s with
	| Some '\n' -> lex s
	| _ -> S.junk s; comment s


let copyRest s =
	let rec f str = try f (cs str (S.next s)) with Stream.Failure -> str
	in f ""

module Ss = Set.Make(Symbol)
type expr =
	| Sym of Symbol.t
	| ParamSym of Symbol.t * expr list
	| Var of Symbol.t
	| Literal of string
	| Seq of expr list
	| Alt of expr list
	| Prec of Symbol.t
	| Code of fragment list
	| ExMacro of expr

let head e = match e with
	| Sym s -> s
	| ParamSym(t,_) -> t
	| _ -> assert false

type grammar = {
	mutable terminals: Ss.t;
	literals: (string, Symbol.t) H.t;
	prec: (Symbol.t, int * assoctype) H.t;
	types: (Symbol.t, string) H.t;
	rules: (Symbol.t, expr * expr * int) H.t;
	macros: (Symbol.t, expr * expr) H.t;
	mutable genrules: LALR.rule list;
	mutable start: Symbol.t option;
	mutable stackout: (expr, bool) H.t
}

let newGrammar () = {
	terminals=Ss.empty;
	literals=H.create 0;
	types=H.create 0;
	prec=H.create 0;
	rules=H.create 0;
	macros=H.create 0;
	genrules=[];
	start=None;
	stackout=H.create 0
}

let parse s g =
	let colon = ref None in
	let preclevel = ref 0 in
	let dtype = ref None in
	let ruleidx = ref 0 in
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
		| t -> error ("expected literal, got "^(tokstr t))
	and expect_type () = match S.next s with
		| TYPE(t) -> t
		| t -> error ("expected type, got "^(tokstr t))
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
			(match !dtype with
			| Some ty -> H.add g.types t ty
			| None -> ());
			p_deftokens ()
		| TYPE(_) -> dtype := Some (expect_type ()); p_deftokens ()
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
	and p_type () =
		match peek () with
		| ID(_) ->
			let t = expect_id () in
			(match !dtype with
			| Some ty -> H.add g.types t ty
			| None -> error "%type without <type>");
			p_type ()
		| TYPE(_) -> dtype := Some (expect_type ()); p_type ()
		| _ -> p_header ()
	and p_header () =
		dtype := None;
		match expect_directive () with
		| "%token" -> p_deftokens ()
		| "%left" -> incr preclevel; p_assoc Left (!preclevel)
		| "%right" -> incr preclevel; p_assoc Right (!preclevel)
		| "%nonassoc" -> incr preclevel; p_assoc Nonassoc (!preclevel)
		| "%type" -> p_type ();
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
		else (
			H.add g.rules (head lhs) (lhs, p_rexpr (), !ruleidx);
			incr ruleidx;
			if g.start = None then
				match lhs with
				| Sym v -> g.start <- Some v
				| _ -> error "invalid start rule"
		);
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
			) else
				Some(r)
		| LPAREN ->
			S.junk s;
			let r = p_rexpr () in
			expect RPAREN;
			Some(r)
		| DIRECTIVE("%prec") -> S.junk s; Some(Prec(expect_id ()))
		| CODE(str) -> S.junk s; Some(Code(str))
		| t -> None
	and p_rules () =
		match peek () with
		| EOF -> ()
		| CODEBLOCK(str) -> S.junk s; footer := !footer ^ str
		| _ ->
			p_rule();
			p_rules()
	in p_header ();
	p_rules ()

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
	| Sym(_) | Var(_) | Literal(_) | Prec(_) | Code(_) -> a
	| ParamSym(t, p) -> ParamSym(t, List.map canonize p)
	| ExMacro x -> ExMacro (canonize x)
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
	| ExMacro(l), ExMacro(l') -> exprMatch scope l l'
	| _ -> false
let rec exprSub g scope e =
	let macro e =
		let rec f ms =
			match ms with
			| [] -> e
			| (lhs,rhs)::t ->
				let scope' = H.create 0 in
				if exprMatch scope' (canonize lhs) (canonize e) then
					ExMacro(exprSub g scope' rhs)
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
	| ExMacro l -> ExMacro (exprSub g scope l)
	| Prec _ | Code _ -> e
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
	| Code(a) -> Format.sprintf "$code({%s})" (fragstr a)
	| ExMacro(a) -> callString("$ex", [a])
let exprToSym e = Symbol.get (exprToString e)

let rec exprExpand e =
	match e with
	| Seq l ->
		let l' = List.map (fun x -> canonize (exprExpand x)) l in
		let rec f left right = match right with
			| [] -> Seq (List.rev left)
			| Alt k::right' -> Alt (List.map (fun x -> Seq [f left []; x; f [] right']) k)
			| x::right' -> f (x::left) right'
		in f [] l'
	| Alt l -> Alt (List.map exprExpand l)
	| Sym _ | Var _ | Literal _ | ParamSym(_, _) | Prec _ | Code _ -> e
	| ExMacro x ->
		match canonize (exprExpand x) with
		| Alt l -> Alt (List.map (fun y -> ExMacro y) l)
		| x' -> ExMacro x'

let rec deseq x =
	match x with
	| Seq l -> (List.map deseq l) |> List.concat
	| _ -> [x]

let rec exprUses e =
	match e with
	| Sym _ | ParamSym(_, _) | Var _ | Literal _ -> [e]
	| Prec _ | Code _ -> []
	| Alt l | Seq l -> List.map exprUses l |> List.concat
	| ExMacro x -> exprUses x

let rec ruleprec g rhs =
	match rhs with
	| [] -> None
	| s::t -> match H.find_opt g.prec s with
		| None -> ruleprec g t
		| Some p -> Some p

let expandRules g start =
	let seen = H.create 0 in
	Ss.iter (fun x -> H.add seen (Sym x) ()) g.terminals;
	let result = ref [] in
	let rec f qu =
		let qu' = ref [] in
		qu |> List.iter (fun p ->
			let found = ref false in
			H.find_all g.rules (head p) |> List.iter (fun (lhs,rhs,idx) ->
				let scope = H.create 0 in
				if exprMatch scope (canonize lhs) (canonize p) then (
					found := true;
					let lhs' = canonize (exprSub g scope lhs) and
					rhs' = canonize (exprExpand (exprSub g scope rhs)) in
					exprUses rhs' |> List.iter (fun e ->
						if not (H.mem seen e) then (
							H.add seen e ();
							qu' := e :: !qu'
						)
					);
					result := (idx, lhs', rhs')::!result
				)
			);
			if not !found then 
				error ("undefined "^(exprToString p));
		);
		if !qu' <> [] then
			f !qu'
	in H.replace seen (Sym start) ();
	f [Sym start];
	List.sort compare !result

let rec exprHasCode e =
	match e with
	| Sym _ | Var _ | ParamSym(_, _) | Literal _ | Prec _ -> false
	| Code _ -> true
	| Alt l | Seq l -> List.exists exprHasCode l
	| ExMacro l -> exprHasCode l

let checkStack g lhs rhs =
	let rec cmp a b =
		if a <> b then
			error ("inconsistent stack use in definition of "^(exprToString lhs))
	and calcOut e =
		match e with
		| Sym _ | ParamSym(_, _) -> H.find g.stackout e
		| Code _ -> true
		| Prec _ -> false
		| Var _ | Literal _ -> assert false
		| Seq l -> List.exists calcOut l
		| Alt l ->
			let l' = List.map calcOut l in
			if l' <> [] then (
				List.iter (cmp (List.hd l')) (List.tl l');
				List.hd l'
			)else
				false
		| ExMacro l -> calcOut l
	in cmp (calcOut rhs) (H.find g.stackout lhs)

let calcStack g list =
	let graph = Grapheval.create (||) in
	list |> List.iter (fun (_,lhs,rhs) -> Grapheval.node graph lhs ((H.mem g.types (head lhs)) || (exprHasCode rhs)));
	g.terminals |> Ss.iter (fun t -> Grapheval.node graph (Sym t) (H.mem g.types t));
	let rec exprProc lhs e =
		match e with
		| Sym _ | ParamSym(_, _) -> Grapheval.edge graph e lhs
		| Literal _ | Var _ | Prec _ | Code _ -> ()
		| Alt l | Seq l -> List.iter (exprProc lhs) l
		| ExMacro l -> exprProc lhs l
	in list |> List.iter (fun (_,lhs,rhs) -> exprProc lhs rhs);
	let h = Grapheval.eval graph in
	g.stackout <- h;
	list |> List.iter (fun (_,lhs,rhs) -> checkStack g lhs rhs);
	list

let rec varmap inputs frags =
	match frags with
	| [] -> []
	| Dat.Var v::t ->
		if v < 1 || v > List.length inputs then
			error ("variable $"^(string_of_int v)^" out of range");
		(List.nth inputs (v-1))::varmap inputs t
	| h::t -> h::varmap inputs t

let combineCodes lhs inputs codes =
	let c' = (List.rev codes |> List.mapi (fun i c ->
		[Piece ("let _c"^(string_of_int (i+1))^" = (\n")]@c@[Piece ") in "]
	) |> List.concat) in
	match List.rev inputs with
	| [] -> None
	| [i] -> Some (c'@[i])
	| Piece _ as p::_ -> Some (c'@[p])
	| _ -> error ("action required for "^(exprToString lhs))

let generateRules g list =
	let idx = ref 1 in
	let addRule lhs rhs prec code =
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
			code
		}::g.genrules;
		incr idx;		
	in let rec stackTranslate lhs rhs stackidx prec =
		let inputs = ref [] in
		let codes = ref [] in
		let rec proc_rhs rhs =
			match rhs with
			| [] -> []
			| ((Sym _ | ParamSym(_, _)) as h)::t ->
				incr stackidx;
				if H.find g.stackout h then
					inputs := !inputs @ [Dat.Var !stackidx];
				h::proc_rhs t
			| Code(a)::t ->
				codes := (varmap !inputs a)::!codes;
				inputs := !inputs  @ [Piece ("_c"^(string_of_int (List.length !codes)))];
				proc_rhs t
			| Prec(p)::t ->
				(match prec with
				| None -> error ("can't define precedence for macro")
				| Some r ->
					if !r = None then (
						r := Some p;
						proc_rhs t
					) else
						error ("precedence of rule "^(exprToString lhs)^" redefined"))
			| ExMacro(x)::t ->
				let (r, c) = stackTranslate lhs (deseq x) stackidx None in
				(match c with
				| None -> ()
				| Some c' ->
					codes := c'::!codes;
					inputs := !inputs  @ [Piece ("_c"^(string_of_int (List.length !codes)))]);
				r @ proc_rhs t
			| (Var _ | Literal _ | Seq _ | Alt _)::_ -> assert false
		in let r = proc_rhs rhs in
		(r, combineCodes lhs !inputs !codes)
	in let rec processRule lhs rhs =
		Format.printf "%s %s\n%!" (exprToString lhs) (exprToString rhs);
		match rhs with
		| Alt(l) -> List.iter (processRule lhs) l
		| Seq(l) ->
			let prec = ref None in
			let stackidx = ref 0 in
			let (r, c) = stackTranslate lhs l stackidx (Some prec) in
			addRule lhs r !prec c
		| _ -> processRule lhs (Seq([rhs]))
	in list |> List.iter (fun (_, lhs, rhs) -> processRule lhs rhs)

let _ =
	(if Array.length Sys.argv < 2 then (Printf.eprintf "usage: %s file\n%!" Sys.argv.(0); exit 1));
	let fname = Sys.argv.(1) in
	ifname := fname;
	let base =
		(if Filename.check_suffix fname ".ss" then
			Filename.chop_suffix fname ".ss"
		else
			fname) |> Filename.basename in
	let f = S.of_channel (open_in Sys.argv.(1)) in
	let toks = S.from (fun _ -> Some (try lex f with Stream.Failure -> EOF)) in
	let g = newGrammar () in
	parse toks g;
	let start = match g.start with Some x -> x | _ -> error "no rules" in
	expandRules g start |> calcStack g |> generateRules g;
	let lalr = LALR.create g.genrules g.prec start in
	LALR.printStates (Format.formatter_of_out_channel (open_out "serpent.output")) lalr;
	let mlname = base ^ "_gen.ml" in
	let ml = Format.formatter_of_out_channel (open_out mlname) in
	Gen.gen_ml mlname ml lalr (Ss.elements g.terminals) g.types;
	Format.fprintf ml "@\n%s@\n" !footer;
	Format.pp_print_flush ml ();
	let mli = Format.formatter_of_out_channel (open_out (base ^ "_gen.mli")) in
	Gen.gen_mli mli lalr (Ss.elements g.terminals) g.types;
	Format.pp_print_flush mli ()
