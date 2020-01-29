type binop =
	| OpAdd
	| OpSub
	| OpMul
	| OpDiv
	| OpMod
	| OpEq
	| OpNe
	| OpLt
	| OpLe
	| OpGt
	| OpGe

type unop =
	| OpUMinus
	| OpVar

type pos = Lexing.position * Lexing.position
type 'a ast_p =
	| Sym of Symbol.t
	| IntLit of int
	| Bin of binop * 'a ast * 'a ast
	| Un of unop * 'a ast
	| Assign of 'a ast * 'a ast
	| Call of 'a ast * 'a ast list
	| Seq of 'a ast list
	| Fix of 'a ast list
	| TypeIs of 'a ast * 'a ast option
	| If of 'a ast * 'a ast * 'a ast
	| While of 'a ast * 'a ast
	| DoWhile of 'a ast * 'a ast
	| For of 'a ast * 'a ast * 'a ast * 'a ast
	| Tuple of 'a ast list
	| Array of 'a ast list
	| Index of 'a ast * 'a ast list
	| Lambda of 'a ast * 'a ast
and 'a ast = {p: 'a ast_p; pos: pos; meta: 'a}
let posed pos p = {p;pos;meta=()}
let repos old p meta = {p; pos=old.pos; meta}

type assoc = Left | Right | Nonassoc

let precStat = 0
let precComma = 1
let precCmp = 2
let precAdd = 3
let precMul = 4
let precUn = 5

let opinfo o =
	match o with
	| OpEq -> ("==", precCmp, Nonassoc)
	| OpNe -> ("!=", precCmp, Nonassoc)
	| OpLt -> ("<", precCmp, Nonassoc)
	| OpLe -> ("<=", precCmp, Nonassoc)
	| OpGt -> (">", precCmp, Nonassoc)
	| OpGe -> (">=", precCmp, Nonassoc)
	| OpAdd -> ("+", precAdd, Left)
	| OpSub -> ("-", precAdd, Left)
	| OpMul -> ("*", precMul, Left)
	| OpDiv -> ("/", precMul, Left)
	| OpMod -> ("%", precMul, Left)

let show t =
	let join sep l = match l with
		| [] -> ""
		| h::t -> List.fold_left (fun a b -> a^sep^b) h t
	in let parens b x = if b then "("^x^")" else x
	in let rec show0 env t =
		match t.p with
		| Sym x -> Symbol.name x
		| IntLit n -> string_of_int n
		| Bin(o,a,b) ->
			let (str, p, ass) = opinfo o in
			let envl = if ass = Right then p else p + 1 in
			let envr = if ass = Left then p else p + 1 in
			parens (env > p) ((show0 envl a)^" "^str^" "^(show0 envr b))
		| Un(OpUMinus,a) -> parens (env > precUn) ("-"^(show0 precUn a))
		| Un(OpVar,a) -> parens (env > precUn) ("var "^(show0 precUn a))
		| Assign(a,b) -> parens (env > 0)
			((show0 (precStat+1) a)^" = "^(show0 (precStat+1) b))
		| Call(a,b) -> (show0 (precUn+1) a)^"("^
			(List.map (show0 (precComma+1)) b |> join ", ")^")"
		| Index(a,b) -> (show0 (precUn+1) a)^"["^
			(List.map (show0 (precComma+1)) b |> join ", ")^"]"
		| Seq(l) -> "("^(List.map (show0 precStat) l |> join ";\n")^")"
		| Tuple(l) -> "("^(List.map (show0 (precComma+1)) l |> join ", ")^")"
		| Array(l) -> "["^(List.map (show0 (precComma+1)) l |> join ", ")^"]"
		| Fix(l) -> "fix(\n"^(List.map (show0 precStat) l |> join ";\n")^")"
		| TypeIs(a,Some b) -> (show0 (precComma+1) a)^" : "^(show0 (precComma+1) b)
		| TypeIs(a,None) -> (show0 (precComma+1) a)^" :"
		| If(a,b,c) -> "if("^(show0 precStat a)^") "^(show0 (precComma+1) b)^" else "^
			(show0 (precComma+1) c)
		| While(a,b) -> "while("^(show0 precStat a)^") "^(show0 (precComma+1) b)
		| DoWhile(a,b) -> "do "^(show0 (precComma+1) a)^" while("^(show0 (precComma+1) b)^")"
		| For(a,b,c,d) ->
			"for"^(show0 precStat a)^";"^(show0 precStat b)^";"^(show0 precStat b)^" "^(show0 (precComma+1) b)
		| Lambda(a,b) -> parens (env > precComma) ((show0 precComma a)^" -> "^(show0 precComma b))
	in show0 0 t

let vcgShow f metaShow t =
	let nctr = ref 0
	in let rec node t label children =
		let id = !nctr in
		incr nctr;
		Format.fprintf f "node: {title:\"%d\"@\nlabel:\"%s\"\ninfo1:\"%s\"\ninfo2:\"%s\"}@\n" id label (Util.locShow t.pos) (metaShow t.meta);
		List.iteri (fun i x -> let id' = g x in
			Format.fprintf f "edge: { sourcename:\"%d\" targetname:\"%d\" horizontal_order:%d }@\n" id id' i
		) children;
		id
	and g t =
		match t.p with
		| Sym x -> node t ("Sym "^(Symbol.name x)) []
		| IntLit n -> node t ("IntLit "^(string_of_int n)) []
		| Bin(o,a,b) -> let (s,_,_) = opinfo o in node t ("Bin "^s) [a;b]
		| Un(OpUMinus,a) -> node t ("Un -") [a]
		| Un(OpVar,a) -> node t ("var") [a]
		| Assign(a,b) -> node t "Assign" [a;b]
		| Call(a,b) -> node t "Call" (a::b)
		| Seq(l) -> node t "Seq" l
		| Fix(l) -> node t "Fix" l
		| TypeIs(a, Some b) -> node t "TypeIs" [a;b]
		| TypeIs(a, None) -> node t "TypeIs" [a]
		| If(a,b,c) -> node t "If" [a;b;c]
		| While(a,b) -> node t "While" [a;b]
		| DoWhile(a,b) -> node t "DoWhile" [a;b]
		| For(a,b,c,d) -> node t "For" [a;b;c;d]
		| Tuple(l) -> node t "Tuple" l
		| Array(l) -> node t "Array" l
		| Index(a,b) -> node t "Index" (a::b)
		| Lambda(a,b) -> node t "Lambda" [a;b]
	in Format.fprintf f "graph: {\ntitle: \"AST\"\n"; ignore (g t); Format.fprintf f "}\n"

let walk (f: 'b ast_p * pos * 'a -> 'b ast) t =
	let rec g t =
		match t.p with
		| Sym s -> f (Sym s, t.pos, t.meta)
		| IntLit n -> f (IntLit n, t.pos, t.meta)
		| Bin(o,a,b) -> f (Bin(o, g a, g b), t.pos, t.meta)
		| Un(o,a) -> f (Un(o, g a), t.pos, t.meta)
		| Assign(a,b) -> f (Assign(g a,g b), t.pos, t.meta)
		| Call(a,b) -> f (Call(g a, List.map g b), t.pos, t.meta)
		| Seq(l) -> f (Seq(List.map g l), t.pos, t.meta)
		| Fix(l) -> f (Fix(List.map g l), t.pos, t.meta)
		| TypeIs(a, Some b) -> f (TypeIs(g a, Some (g b)), t.pos, t.meta)
		| TypeIs(a, None) -> f (TypeIs(g a, None), t.pos, t.meta)
		| If(a,b,c) -> f (If(g a, g b, g c), t.pos, t.meta)
		| While(a,b) -> f (While(g a, g b), t.pos, t.meta)
		| DoWhile(a,b) -> f (DoWhile(g a, g b), t.pos, t.meta)
		| For(a,b,c,d) -> f (For(g a, g b, g c, g d), t.pos, t.meta)
		| Tuple(l) -> f (Tuple(List.map g l), t.pos, t.meta)
		| Array(l) -> f (Array(List.map g l), t.pos, t.meta)
		| Index(a, b) -> f (Index(g a, List.map g b), t.pos, t.meta)
		| Lambda(a, b) -> f (Lambda(g a, g b), t.pos, t.meta)
	in g t

let visit (f:'b ast_p * pos * 'a -> unit) t =
	walk (fun (p, pos, meta) -> f (p, pos, meta); {p; pos; meta}) t
