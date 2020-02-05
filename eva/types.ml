module H = Hashtbl
open Ast
open Type

type meta = {
	mutable typ: typ
}

let analyse t =
	let symtab = H.create 0 in
	List.iter (fun (a,b,c) -> match b with
		| TypeLit t -> H.add symtab a t
		| _ -> ()) Builtin.vars;
	let unktype () = TypeVar (Symbol.temp ("$t")) in
	let rec subst a =
		match a with
		| TypeVar s -> (match H.find_opt symtab s with
			| Some t -> subst t
			| None -> a)
		| Int | String | Bool | Unit | Type -> a
		| Tuple l -> Tuple (List.map subst l)
		| Array x -> Array (subst x)
		| Fun(a,b) -> Fun(List.map subst a, subst b)
	in let rec unify pos (a:typ) (b:typ) = 
		let a' = subst a and b' = subst b in
		match a', b' with
		| Int, Int -> Int
		| Bool, Bool -> Bool
		| String, String -> String
		| Unit, Unit -> Unit
		| Fun(a, b), Fun(c, d) when List.length a = List.length c ->
			Fun(List.map2 (unify pos) a c, unify pos b d)
		| Tuple a, Tuple b when List.length a = List.length b ->
			Tuple(List.map2 (unify pos) a b)
		| Array a, Array b -> Array(unify pos a b)
		| TypeVar s, TypeVar s' when s = s' -> TypeVar s
		| TypeVar s, t -> H.replace symtab s t; t
		| t, TypeVar s -> H.replace symtab s t; t
		| _ -> Util.error pos ("type error: "^(Astutil.typshow a')^" and "^(Astutil.typshow b'))
	in let rec dotype (t:unit ast) =
		let node p = {p; pos=t.pos; meta={typ=Type}} in
		match t.p with
		| Sym s ->
			let t' = subst (TypeVar s) in
			(node (Sym s), t')
		| _ -> Util.error t.pos ("invalid type "^(Vcg.astname t))
	and dolval (t:unit ast) =
		let node p typ = {p; pos=t.pos; meta={typ}} in
		match t.p with
		| Sym s -> (match H.find_opt symtab s with
			| Some x -> node (Sym s) x
			| None -> node (Sym s) (TypeVar s))
		| TypeIs(a, Some b) ->
			let a' = doexpr a and (_, t') = dotype b in
			ignore (unify t.pos a'.meta.typ t');
			a'
		| TypeIs(a, None) -> dolval a
		| Tuple l ->
			let l' = List.map dolval l in
			let t' = Tuple (List.map (fun x -> x.meta.typ) l') in
			node (Tuple(l')) t'
		| _ -> Util.error t.pos ("invalid lval "^(Vcg.astname t))
	and doexpr (t:unit ast) =
		let node p typ = {p; pos=t.pos; meta={typ}} in
		let implicit a t' = a.meta.typ <- unify a.pos a.meta.typ t'; a in
		match t.p with
		| IntLit n -> node (IntLit n) Int
		| TypeLit t' -> node (TypeLit t') Type
		| Fix l -> node (Astutil.map doexpr t) Unit
		| Sym s ->
			(match H.find_opt symtab s with
			| Some x -> node (Sym s) x
			| None -> node (Sym s) (TypeVar s))
		| Assign(a,b) ->
			let a' = dolval a and b' = doexpr b in
			node (Assign(a', implicit b' a'.meta.typ)) Unit
		| Seq [] -> node (Seq[]) Unit
		| Seq l ->
			(match List.rev l with [] -> assert false |
			e::l ->
			let e' = doexpr e in
			let l' = List.map (fun x -> implicit (doexpr x) Unit) l in
			node (Seq(e'::l')) e'.meta.typ)
		| If(a,b,c) ->
			let a' = doexpr a and b' = doexpr b and c' = doexpr c in
			let t = unify t.pos b'.meta.typ c'.meta.typ in
			node (If(implicit a' Bool, implicit b' t, implicit c' t)) t
		| While(a,b) ->
			let a' = doexpr a and b' = doexpr b in
			node (While(implicit a' Bool, implicit b' Unit)) Unit
		| DoWhile(a,b) ->
			let a' = doexpr a and b' = doexpr b in
			node (While(implicit a' Unit, implicit b' Bool)) Unit
		| For(a,b,c,d) ->
			let a' = doexpr a and b' = doexpr b and c' = doexpr c and d' = doexpr d in
			node (For(implicit a' Unit, implicit b' Bool, implicit c' Unit, implicit d' Unit)) Unit
		| TypeIs(a, Some b) ->
			let a' = doexpr a and (_, t') = dotype b in
			implicit a' t'
		| TypeIs(a, None) -> doexpr a
		| Tuple l ->
			let l' = List.map doexpr l in
			let t' = Tuple (List.map (fun x -> x.meta.typ) l') in
			node (Tuple(l')) t'
		| Lambda(a, b) -> 
			let a' = dolval a and b' = doexpr b in
			(match a'.meta.typ with
			| Tuple l -> node(Lambda(a', b')) (Fun(l, b'.meta.typ))
			| _ -> node(Lambda(a', b')) (Fun([a'.meta.typ], b'.meta.typ)))
		| Call(a, b) ->
			let a' = doexpr a and b' = List.map doexpr b and rt = unktype() in
			let t' = Fun(List.map (fun x -> x.meta.typ) b', rt) in
			(match unify t.pos a'.meta.typ t' with
			| Fun(p, q) ->
				node (Call(implicit a' (Fun(p,q)), List.map2 implicit b' p)) rt
			| _ -> assert false)
		| Bin(o,a,b) ->
			let a' = doexpr a and b' = doexpr b in
			(match o with
			| OpAdd | OpSub | OpMul | OpDiv | OpMod ->
				node (Bin(o,implicit a' Int,implicit b' Int)) Int
			| OpEq | OpNe
			| OpLt | OpLe | OpGt | OpGe ->
				let t = unify t.pos a'.meta.typ b'.meta.typ in
				node (Bin(o,implicit a' t,implicit b' t)) Bool
			)
		| Un(o,a) ->
			let a' = doexpr a in
			node (Un(o,implicit a' Int)) Int
		| Let(a, None) ->
			let a' = doexpr a in
			node (Let(a', None)) Unit
		| Let(a, Some b) ->
			let a' = doexpr a and b' = doexpr b in
			node (Let(a', Some b')) b'.meta.typ
		| Array l ->
			let l' = List.map doexpr l in
			(match l' with
			| [] -> node(Array[]) (Array(unktype()))
			| h::r ->
				let t' = List.fold_left (fun x y -> unify t.pos x y.meta.typ) h.meta.typ r in
				node (Array (List.map (fun x -> implicit x t') l'))
					(Array t'))
	in let t' =
		doexpr t
		|> Astutil.walk (fun (p,pos,{typ}) -> {p;pos;meta={typ=subst typ}})
	in
	Vcg.astshow (Format.std_formatter) (fun t -> Astutil.typshow t.meta.typ) t';
	t'
