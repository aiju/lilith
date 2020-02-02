module H = Hashtbl
open Dat

type state = {
	id: int;
	next: (Symbol.t, state) H.t;
	prev: (Symbol.t, state) H.t;
	actions: (Symbol.t, action) H.t;
	mutable dist: int;
	mutable path: Symbol.t list
}
module State = struct type t = state let compare a b = compare a.id b.id end
module Sst = Set.Make(State)

module StateDist = struct
	type t = state
	let compare a b =
		if a.dist = b.dist then
			compare a.id b.id
		else
			compare a.dist b.dist
end
module Sdist = Set.Make(State)

module Result = struct
	type t = int * Symbol.t list
	let compare (a, b) (a', b') =
		if a = a' then
			compare b b'
		else
			compare a a'
end
module Sres = Set.Make(Result)

let dijkstra st =
	let h = ref (Sdist.add st Sdist.empty) in
	let explored = ref Sst.empty in
	let rec f () =
		let u = Sdist.min_elt !h in
		h := Sdist.remove u !h;
		explored := Sst.add u !explored;
		u.next |> H.iter (fun e v ->
			if not (Sst.mem v !explored) then (
				if not (Sdist.mem v !h) || u.dist + 1 < v.dist then (
					h := Sdist.remove v !h;
					v.dist <- u.dist + 1;
					v.path <- e::u.path;
					h := Sdist.add v !h
				)
			)
		);
		if not (Sdist.is_empty !h) then
			f ()
	in f ()

let analyseConflicts lalr =
	let (sr, rr) = LALR.findconflicts lalr in
	if H.length sr > 0 || H.length rr > 0 then (
	let nstates = H.fold (fun (id,_) _ c -> max c (id+1)) (LALR.actions lalr) 0 in
	let states = Array.init nstates (fun id -> {id; next= H.create 0; prev= H.create 0; actions= H.create 0; dist= 0; path= []}) in
	LALR.actions lalr |> H.iter (fun (id, e) a ->
		H.add states.(id).actions e a;
		match a with
		| Shift id' ->
			H.add states.(id).next e states.(id');
			H.add states.(id').prev e states.(id)
		| _ -> ());
	LALR.goto lalr |> H.iter (fun (id, e) id' ->
		H.add states.(id).next e states.(id');
		H.add states.(id').prev e states.(id));
	let rec reduceRule (id,e) idx = 
		match H.find_all (LALR.actions lalr) (id,e)
		|> List.filter (fun r ->
			match r with
			| Reduce(idx', _, _, _) when idx=idx' -> true
			| _ -> false
		) with
		| [Reduce(_,lhs,rhs,_)] -> (lhs,rhs)
		| _ -> assert false
	and undoReduce st rhs =
		let rec f l st = match l with
			| [] -> Sst.add st Sst.empty
			| h::t -> List.map (f t) (H.find_all st.prev h) |> List.fold_left Sst.union Sst.empty
		in f (List.rev rhs) st
	and fullReduce st (lhs,rhs) e stack seen result =
		if not (Sst.mem st seen) then (
			undoReduce st rhs |> Sst.iter (fun st' -> 
				let st'' = H.find st'.next lhs in
				match H.find_opt st''.actions e with
				| Some (Shift _) | Some Accept ->
					let l = List.rev_append st'.path (List.rev stack) in
					result := Sres.add (List.length l, l) !result
				| None | Some Error -> ()
				| Some (Reduce (_, lhs', rhs', _)) ->
					fullReduce st'' (lhs', rhs') e (stack @ (List.tl (List.rev rhs'))) (Sst.add st seen) result
			);
		)
	and analyseShiftReduce id (e, shift, reduce) =
		let state = states.(id) in
		let (lhs, rhs) = reduceRule (id,e) reduce in
		let result = ref Sres.empty in
		fullReduce state (lhs,rhs) e (List.rev rhs) Sst.empty result;
		let (_, l) = Sres.min_elt !result in
		List.iter (Format.printf "%a " Symbol.pp) l;
		Format.printf ". %a ...\n%!" Symbol.pp e;
	and analyseReduceReduce id (e, reduce, reduce') =
		let state = states.(id) in
		let (lhs, rhs) = reduceRule (id, e) reduce in
		let (lhs', rhs') = reduceRule (id, e) reduce' in
		let result = ref Sres.empty and result' = ref Sres.empty in
		fullReduce state (lhs,rhs) e (List.rev rhs) Sst.empty result;
		fullReduce state (lhs',rhs') e (List.rev rhs') Sst.empty result';
		let s = Sres.inter !result !result' in
		if Sres.is_empty s then
			Format.printf "failure\n%!"
		else (
			let (_, l) = Sres.min_elt s in
			List.iter (Format.printf "%a " Symbol.pp) l;
			Format.printf ". %a ...\n%!" Symbol.pp e;
		)
	in
	dijkstra states.(0);
	H.iter analyseShiftReduce sr;
	H.iter analyseReduceReduce rr
	)
