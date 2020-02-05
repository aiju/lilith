open Ast
open Type

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

let rec typshow t =
	match t with
	| Unit -> "unit"
	| Int -> "int"
	| String -> "string"
	| Type -> "type"
	| Bool -> "bool"
	| Tuple l -> "(" ^ (String.concat "," (List.map typshow l)) ^ ")"
	| Array t -> "[" ^ (typshow t) ^ "]"
	| Fun(a,b) -> "(" ^ (String.concat "," (List.map typshow a)) ^ ") -> " ^ (typshow b)
	| TypeVar s -> Symbol.name s

let show t =
	let join sep l = match l with
		| [] -> ""
		| h::t -> List.fold_left (fun a b -> a^sep^b) h t
	in let parens b x = if b then "("^x^")" else x
	in let rec show0 env t =
		match t.p with
		| Sym x -> Symbol.name x
		| IntLit n -> string_of_int n
		| TypeLit t' -> typshow t'
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
		| Let(a,Some b) -> parens (env > precComma) ("let " ^ (show0 precComma a) ^ " in " ^ (show0 precComma b))
		| Let(a,None) -> parens (env > precComma) ("let " ^ (show0 precComma a))
	in show0 0 t

let map (f: 'a ast -> 'b ast) t =
	match t.p with
	| Sym s -> Sym s
	| IntLit n -> IntLit n
	| TypeLit t' -> TypeLit t'
	| Bin(o,a,b) -> Bin(o, f a, f b)
	| Un(o,a) -> Un(o, f a)
	| Assign(a,b) -> Assign(f a, f b)
	| Call(a,b) -> Call(f a, List.map f b)
	| Seq(l) -> Seq(List.map f l)
	| Fix(l) -> Fix(List.map f l)
	| TypeIs(a, b) -> TypeIs(f a, Util.optionMap f b)
	| If(a,b,c) -> If(f a, f b, f c)
	| While(a,b) -> While(f a, f b)
	| DoWhile(a,b) -> DoWhile(f a, f b)
	| For(a,b,c,d) -> For(f a, f b, f c, f d)
	| Tuple(l) -> Tuple(List.map f l)
	| Array(l) -> Array(List.map f l)
	| Index(a, b) -> Index(f a, List.map f b)
	| Lambda(a, b) -> Lambda(f a, f b)
	| Let(a, b) -> Let(f a, Util.optionMap f b)

let fold (f: 'a ast -> 'b list -> 'b list) x t =
	let f' a b = f b a in
	match t.p with
	| Sym _ -> x
	| IntLit _ -> x
	| TypeLit _ -> x
	| Bin(o,a,b) -> x |> f a |> f b
	| Un(o, a) -> x |> f a
	| Assign(a, b) -> x |> f a |> f b
	| Call(a, b) -> List.fold_left f' (f a x) b
	| Seq(l) -> List.fold_left f' x l
	| Fix(l) -> List.fold_left f' x l
	| TypeIs(a, b) -> Util.optionFold f (f a x) b
	| If(a, b, c) -> x |> f a |> f b |> f c
	| While(a, b) -> x |> f a |> f b
	| DoWhile(a, b) -> x |> f a |> f b
	| For(a,b,c,d) -> x |> f a |> f b |> f c |> f d
	| Tuple(l) -> List.fold_left f' x l
	| Array(l) -> List.fold_left f' x l
	| Index(a, b) -> List.fold_left f' (f a x) b
	| Lambda(a, b) -> x |> f a |> f b
	| Let(a, b) -> Util.optionFold f (f a x) b

let iter (f: 'a ast -> unit) t =
	ignore (map (fun x -> f x; x) t)

let rec visit f t =
	f t;
	iter (visit f) t

let rec walk f t =
	f (map (walk f) t, t.pos, t.meta)

let children t = List.rev (fold (fun a b -> a::b) [] t)
