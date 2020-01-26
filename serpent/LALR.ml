module H = Hashtbl
module G = Grapheval
open Dat

type rule = {
	lhs: Symbol.t;
	rhs: Symbol.t list;
	prec: (int * assoctype) option;
	idx: int;
	action: string option
}

module Ss = Set.Make(Symbol)
let endSym = Symbol.get("$end")
let startSym = Symbol.get("$accept")
type item = Item of int * Symbol.t * Symbol.t list * Symbol.t list
type state = State of {
	id: int;
	set: item list;
	edges: (Symbol.t, state) H.t;
	reduce: (Symbol.t, int * Symbol.t * Symbol.t list) H.t
}
type t = {
	rules: (Symbol.t, rule) H.t;
	terminals: Symbol.t list;
	nonterminals: Symbol.t list;
	prectab: (Symbol.t, int * assoctype) H.t;
	mutable states: state array;
	statetab: (item list, int) H.t;
	mutable nullable: (Symbol.t, bool) H.t;
	mutable first: (Symbol.t, Ss.t) H.t;
	mutable follow: (Symbol.t, Ss.t) H.t;
	mutable actions: (int * Symbol.t, action) H.t
}
let isNonterminal g t = H.mem g.rules t
let isTerminal g t = not (isNonterminal g t)

let calcNullable g =
	let rhslook = H.create 0 in
	let nullable = H.create 0 in
	List.iter (fun x -> H.replace nullable x false) (List.rev_append g.nonterminals g.terminals);
	let qu = ref [] in
	H.iter (fun lhs {rhs} ->
		List.iter (fun x -> H.add rhslook x (lhs,rhs)) rhs;
		if rhs = [] then qu := (lhs,rhs)::!qu;
	) g.rules;
	let rec nullableLoop l =
		let l' = ref [] in
		List.iter (fun (lhs,rhs) ->
			if List.for_all (H.find nullable) rhs then (
				H.replace nullable lhs true;
				l' := List.rev_append (H.find_all rhslook lhs) !l' 
			)
		) l;
		if !l' <> [] then nullableLoop !l'
	in nullableLoop !qu;
	g.nullable <- nullable

type firstfollow = First of Symbol.t | Follow of Symbol.t
let calcFirstFollow g =
	let graph = G.create Ss.union in
	List.iter (fun x ->
		G.node graph (First x) (Ss.add x Ss.empty);
		G.node graph (Follow x) Ss.empty
	) g.terminals;
	List.iter (fun x ->
		G.node graph (First x) Ss.empty;
		G.node graph (Follow x) Ss.empty
	) g.nonterminals;
	H.iter (fun lhs {rhs} ->
		let rec f l f' = match l with [] -> ()
			| y::t ->
				f' y lhs;
				if H.find g.nullable y then
					f t f'
		in f rhs (fun y lhs -> G.edge graph (First y) (First lhs));
		f (List.rev rhs) (fun y lhs -> G.edge graph (Follow lhs) (Follow y));
		let rec f l = match l with [] -> () | y::t -> f' y t; f t
		and f' y l = match l with [] -> () | y'::t' ->
			G.edge graph (First y') (Follow y);
			if H.find g.nullable y' then
				f' y t'
		in f rhs
	) g.rules;
	let first = H.create 0 and follow = H.create 0 in
	H.iter (fun x y -> match x with
		| First n -> H.add first n y
		| Follow n -> H.add follow n y
	) (G.eval graph);
	g.first <- first;
	g.follow <- follow

let rec first g l = match l with [] -> Ss.empty | h::t ->
	let f = H.find g.first h in
	if H.find g.nullable h then
		Ss.union f (first g t)
	else
		f

let rec closure g i n =
	let n' = ref [] in
	List.iter (fun s -> H.add i s ()) n;
	List.iter (fun it -> match it with
		| Item(_, _, _, x::t) ->
			H.find_all g.rules x |>
			List.iter (fun {idx;rhs=j}->
				let s = Item(idx, x, [], j) in
				match H.find_opt i s with
				| None ->
					n' := s::!n'
				| Some _ -> ())
		| _ -> ()) n;
	if !n' <> [] then
		closure g i !n'

let deoption def o = match o with Some x -> x | None -> def
let hmodify t k def f =
	match H.find_opt t k with
	| None -> H.replace t k (f def)
	| Some x -> H.replace t k (f x)
let hkeys t =
	let l = ref [] in
	H.iter (fun x _ -> l := x::!l) t;
	List.sort_uniq compare !l

let rec getState g l =
	let hset = H.create 0 in
	closure g hset l;
	let set = hkeys hset in
	match H.find_opt g.statetab set with
	| Some id -> g.states.(id)
	| None ->
		let id = Array.length g.states in
		let edges = H.create 0 in
		let s = State{id; set; edges; reduce=H.create 0} in
		g.states <- Array.append g.states [|s|];
		H.add g.statetab set id;
		let next = H.create 0 in
		List.iter (fun it ->
			match it with
			| Item(idx, lhs, left, x::right) ->
				hmodify next x [] (fun a->Item(idx, lhs, left@[x], right)::a)
			| _ -> ()) set;
		H.iter (fun x j ->
			if x = endSym then
				H.add edges x s
			else
				H.add edges x (getState g j)
		) next;
		s

let iterNonterminalEdge g f =
	g.states |> Array.iter (fun (State{id;edges}) ->
		edges |> H.iter (fun t _ ->
			if isNonterminal g t then
				f (id,t))
	)

type lnode = Read of int * Symbol.t | Follow of int * Symbol.t | LA of int * int * Symbol.t * Symbol.t list
let lnode_pp f n = match n with
	| Read(p,a) -> Format.fprintf f "Read(%d,%a)" p Symbol.pp a
	| Follow(p,a) -> Format.fprintf f "Follow(%d,%a)" p Symbol.pp a
	| LA(p,idx,a,rhs) -> Format.fprintf f "LA(%d,%d,%a,[%a])" p idx Symbol.pp a (Format.pp_print_list ?pp_sep: (Some (fun f () -> Format.fprintf f ",")) Symbol.pp) rhs
let set_pp f s = Ss.iter (Format.fprintf f "%a " Symbol.pp) s

(* See Frank DeRemer and Thomas Pennello, Efficient Computation of LALR(1) Look-Ahead Sets *)
let calcLALR g =
	let graph = G.create Ss.union in
	let edges p = let State{edges} = g.states.(p) in edges in
	let target (p,a) = let State{id} = H.find (edges p) a in id in
	let directreads (p,a) =
		let e = edges (target (p,a)) in
		H.fold (fun t _ s ->
			if isTerminal g t then Ss.add t s
			else s) e Ss.empty in
	iterNonterminalEdge g (fun (p,a) ->
		G.node graph (Read(p,a)) (directreads(p,a));
		G.node graph (Follow(p,a)) Ss.empty;
		G.edge graph (Read(p,a)) (Follow(p,a))
	);
	iterNonterminalEdge g (fun (p,a) ->
		let r = target (p,a) in
		edges r |> H.iter (fun c _ ->
			if H.find g.nullable c then
				G.edge graph (Read(r,c)) (Read(p,a))
		)
	);
	iterNonterminalEdge g (fun (p',b) ->
		H.find_all g.rules b |> List.iter (fun {rhs} ->
			let rec findStates p'' l acc = match l with [] -> acc
				| a::t -> let p''' = target (p'',a) in findStates p''' t (p''::acc)
			in let states = findStates p' rhs [] in
			let rec findIncludes l sl = match (l,sl) with [],_|_,[] -> ()
				| a::t, p::t' ->
					if isNonterminal g a then (
						G.edge graph (Follow(p',b)) (Follow(p,a));
						if H.find g.nullable a then
							findIncludes t t'
					)
			in findIncludes (List.rev rhs) states
		)
	);
	g.states |> Array.iter (fun (State{id;set}) ->
		set |> List.iter (fun it -> match it with
			| Item(idx, lhs,rhs,[]) ->
				G.node graph (LA(id, idx, lhs, rhs)) Ss.empty
			| _ -> ()));
	iterNonterminalEdge g (fun (p,a) -> 
		H.find_all g.rules a |> List.iter (fun {idx;rhs} ->
			let q = List.fold_left (fun q b -> target (q,b)) p rhs in
			G.edge graph (Follow(p, a)) (LA(q, idx, a, rhs))
		)
	);
	let h = G.eval graph in
	H.iter (fun a b ->
		match a with
		| LA(a,idx,lhs,rhs) ->
			let State{reduce} = g.states.(a) in
			Ss.iter (fun x -> H.add reduce x (idx,lhs,rhs)) b
		| _ -> ()
	) h

let shiftreduce g state token shift (idx,lhs,rhs) =
	let {prec=ruleprec} = match H.find_all g.rules lhs |> List.filter (fun {idx=idx'} -> idx=idx') with
		| [t] -> t
		| _ -> assert false in
	let tokenprec = H.find_opt g.prectab token in
	match ruleprec, tokenprec with
	| None, _ | _, None ->
		H.add g.actions (state, token) (Reduce (idx,lhs,rhs))
	| Some(l, _), Some(l', _) when l < l' -> H.replace g.actions (state, token) (Shift shift)
	| Some(l, _), Some(l', _) when l > l' -> H.replace g.actions (state, token) (Reduce(idx,lhs,rhs))
	| Some(_, Left), _ -> H.replace g.actions (state, token) (Reduce(idx, lhs, rhs))
	| Some(_, Right), _ -> H.replace g.actions (state, token) (Shift shift)
	| Some(_, Nonassoc), _ -> H.replace g.actions (state, token) Error

let findconflicts g =
	let sr = H.create 0 and rr = H.create 0 in
	g.actions |> H.iter (fun (id, e) a ->
		H.find_all g.actions (id,e) |> List.iter (fun a' ->
			match (a,a') with
			| Shift n, Reduce (idx,_,_) -> H.add sr id (e, n, idx)
			| Reduce(idx,_,_), Reduce(idx',_,_) when idx < idx' -> H.add rr id (e, idx, idx')
			| _ -> ()
		)
	);
	(sr, rr)

let genTables g =
	Array.iter (fun (State{id;edges;reduce}) ->
		H.iter (fun e (State{id=id'}) ->
			if e = endSym then
				H.replace g.actions (id, e) Accept
			else
				H.replace g.actions (id, e) (if isTerminal g e then Shift id' else Goto id')
		) edges;
		H.iter (fun e (idx,lhs,rhs) ->
			match H.find_opt g.actions (id, e) with
			| Some (Shift n) -> shiftreduce g id e n (idx,lhs,rhs)
			| _ -> H.add g.actions (id, e) (Reduce (idx,lhs,rhs))
		) reduce
	) g.states;
	let (sr, rr) = findconflicts g in
	Format.printf "%d shift/reduce, %d reduce/reduce@\n%!" (H.length sr) (H.length rr)
	
let create rules' prectab start =
	let rules = H.create 0 in
	H.add rules startSym {idx=0; lhs=startSym; rhs=[start; endSym]; prec=None; action=None};
	List.iter (fun r -> H.add rules r.lhs r) rules';
	let nonterminals = hkeys rules in
	let terminals = H.fold (fun _ {rhs=b} c -> List.fold_left (fun c x ->
		if not (H.mem rules x) then
			List.merge compare [x] c
		else c) c b) rules [] in
	let g = {
		rules;
		states=[||];
		statetab=H.create 0;
		nonterminals;
		terminals;
		first=H.create 0;
		follow=H.create 0;
		nullable=H.create 0;
		actions=H.create 0;
		prectab
	} in
	calcNullable g;
	calcFirstFollow g;
	ignore (getState g [Item(0, startSym, [], [start; endSym])]);
	calcLALR g;
	genTables g;
	g

let printStates f g =
	let tabulate s e t =
		let w = List.fold_left (fun c l -> List.map2 max c (List.map String.length l))
				(List.map String.length (List.hd t)) (List.tl t) in
		List.iter (fun r -> Format.fprintf f s; List.iter2 (Format.fprintf f "%-*s") w r; Format.fprintf f e) t
	in
	H.fold (fun lhs {idx;rhs} c -> (idx,lhs,rhs)::c) g.rules []
	|> List.sort (fun (idx,_,_) (idx',_,_) -> compare idx idx')
	|> List.iter (fun (idx,lhs,rhs) ->
		Format.fprintf f "%4d  %a : " idx Symbol.pp lhs;
		List.iter (Format.fprintf f "%a " Symbol.pp) rhs;
		Format.fprintf f "@\n"
	);
	Format.fprintf f "@\n";
	let (sr, rr) = findconflicts g in
	Array.iter (fun (State{id;set;reduce}) ->
		H.find_all sr id |> List.iter (fun (a,b,c) ->
				Format.printf "%d: shift/reduce conflict on %a (shift %d, reduce %d)@\n" id Symbol.pp a b c);
		H.find_all rr id |> List.iter (fun (a,b,c) ->
				Format.printf "%d: reduce/reduce conflict on %a (shift %d, reduce %d)@\n" id Symbol.pp a b c);
		Format.fprintf f "State %d@\n" id;
		List.map (fun (Item(idx,lhs,left,right)) ->
			[Symbol.name lhs;
				" : ";
				List.fold_left (fun x y -> x ^ y ^ " ") ""
					((List.map Symbol.name left)@["."]@(List.map Symbol.name right));
				" ("^(string_of_int idx)^")"]
		) set |> tabulate "\t" "@\n";
		Format.fprintf f "@\n";
		
		let a = (H.fold (fun (s, e) a c ->
			match a with
			| Shift(s') when s=id -> [Symbol.name e; "  shift " ^ (string_of_int s')] :: c
			| Accept when s=id -> [Symbol.name e; "  accept"] :: c
			| _ -> c) g.actions [] |> List.sort compare) @
		(H.fold (fun (s, e) a c ->
			match a with
			| Reduce(idx,_,_) when s=id -> [Symbol.name e; "  reduce " ^ (string_of_int idx)] :: c
			| _ -> c) g.actions [] |> List.sort compare)
		and b = (H.fold (fun (s, e) a c ->
			match a with
			| Goto(s') when s=id -> [Symbol.name e; "  goto " ^ (string_of_int s')] :: c
			| _ -> c) g.actions [] |> List.sort compare) in
		let c = a @ (if a <> [] && b <> [] then [["";""]] else []) @ b in
		tabulate "\t" "@\n" c;
		Format.fprintf f "@\n@\n"
	) g.states
