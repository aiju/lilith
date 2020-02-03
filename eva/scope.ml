module H = Hashtbl
open Ast

module Def = struct type t = Symbol.t * Symbol.t let compare = compare end
module Sd = Set.Make(Def)

type blocktype = BSeq | BFix | BLet

type block = {
	bid: int;
	typ: blocktype;
	mutable ast: meta ast list;
	up: block option;
	mutable sub: block list;
	mutable next: block list;
	mutable prev: block list;
	mutable defs: Sd.t;
	mutable scopein: Sd.t;
	mutable scopeout: Sd.t
}
and meta = {
	id: int;
	block: block;
	repl: Symbol.t option
}

let idalloc = Util.idAllocator ()
let bidalloc = Util.idAllocator()
let newblock typ up = 
	let b = {
		ast=[];
		bid=bidalloc();
		next=[];
		prev=[];
		up;
		typ;
		sub=[];
		defs=Sd.empty;
		scopein=Sd.empty;
		scopeout=Sd.empty
	} in
	(match up with
	| Some b' -> b'.sub <- b::b'.sub
	| None -> ());
	b
let addast b s = b.ast <- s::b.ast
let edge bl bl' = bl |> List.iter (fun b -> bl' |> List.iter (fun b' ->
	b.next <- b'::b.next;
	b'.prev <- b::b'.prev))

let rec processLval up colon lhs  =
	let node x = let t' = {p=x; pos=lhs.pos; meta={id=idalloc(); block=up; repl=None}} in addast up t'; t' in
	match lhs.p with
	| TypeIs(a, Some b) -> 
		let a' = processLval up true a in
		let (b', _, _) = blockify up b in
		node (TypeIs(a', Some b'))
	| TypeIs(a, None) ->
		node (TypeIs(processLval up true a, None))
	| Sym(s) ->
		if colon then (
			let temp = Symbol.temp ((Symbol.name s) ^ "$") in
			up.defs <- Sd.add (s, temp) up.defs;
			let t' = {p=Sym s; pos=lhs.pos; meta={id=idalloc(); block=up; repl=Some temp}} in
			addast up t';
			t'
		)else
			node (Sym s)
	| Un(OpVar, a) -> node (Un(OpVar, processLval up colon a))
	| _ -> Util.error(lhs.pos) ("invalid lval "^(Vcg.astname lhs))

and blockify up t : meta ast * block list * block list =
	let node x = let t' = {p=x; pos=t.pos; meta={id=idalloc(); block=up; repl=None}} in addast up t'; t' in
	let h x = blockify (newblock BSeq (Some up)) x in
	match t.p with
	| Seq (_::_ as l) ->
		let l' = List.map h l in
		Util.iterAdj (fun (_, _, a) (_, b, _) -> edge a b) l';
		(node (Seq (List.map (fun (a, _, _) -> a) l')), [up], [up])
	| Fix l ->
		let b = newblock BFix (Some up) in
		let l' = List.map (fun x -> let (e, _, _) = blockify b x in e) l in
		(node (Fix l'), [up], [up])
	| Lambda(a,b) ->
		let block = newblock BLet (Some up) in
		let lhsblock = newblock BSeq (Some block) in
		let a' = processLval lhsblock true a
		and (b', bin, _) = blockify (newblock BSeq (Some block)) b in
		edge [lhsblock] bin;
		(node (Lambda (a', b')), [up], [up])
	| Let(a,Some b) ->
		let lhs = newblock BLet (Some up) in
		let (a', _, bout) = blockify lhs a
		and (b', bin, _) = h b in
		edge bout bin;
		(node (Let(a', Some b')), [up], [up])
	| Let(a,None) ->
		let block = newblock BLet up.up in
		let (a', bin, bout) = blockify block a in
		edge bout [up];
		(node (Let(a', None)), bin, [up])
	| If(a,b,c) ->
		let (a', ain, aout) = h a and
		(b', bin, bout) = h b and
		(c', cin, cout) = h c in
		edge aout bin;
		edge aout cin;
		(node (If(a', b', c')), [up], [up])
	| While(a, b) -> 
		let (a', ain, aout) = h a and
		(b', bin, bout) = h b in
		edge aout bin;
		edge bout ain;
		(node (While(a', b')), [up], [up])
	| DoWhile(a, b) -> 
		let (a', ain, aout) = h a and
		(b', bin, bout) = h b in
		edge aout bin;
		edge bout ain;
		(node (DoWhile(a', b')), [up], [up])
	| For(a, b, c, d) ->
		let (a', ain, aout) = h a and
		(b', bin, bout) = h b and
		(c', cin, cout) = h c and
		(d', din, dout) = h d in
		edge aout bin;
		edge bin din;
		edge dout cin;
		edge cout bin;
		(node (For(a', b', c', d')), [up], [up])
	| Assign(a, b) ->
		let a' = processLval up false a and
		(b', _, _) = blockify up b in
		(node (Assign(a', b')), [up], [up])
	| Seq []
	| Bin(_, _, _)
	| Un(_, _)
	| Sym _
	| IntLit _
	| Call(_, _)
	| Index(_, _)
	| Array(_)
	| Tuple(_) 
	| TypeIs(_, _) ->
		let f x =
			let (e, _, _) = blockify up x in
			e in
		(node (Astutil.map f t), [up], [up])

let propagate b =
	let blocks = (
		let ret = ref [b] in
		let rec findblocks b =
			ret := b.sub @ !ret;
			List.iter findblocks b.sub
		in findblocks b;
		!ret
	) in
	let changed = ref true in
	let chCmp a b =
		(if not (Sd.equal a b) then changed := true);
		b in
	let chUnion a b = chCmp a (Sd.union a b) in
	while !changed do
		changed := false;
		blocks |> List.iter (fun b ->
			b.sub |> List.iter (fun b' ->
				if b'.typ <> BLet then
					b.defs <- chUnion b.defs b'.defs
			);
			(match b.prev, b.up with
			| [], Some b' ->
				b.scopein <- b'.scopein
			| _ ->
				let s = List.fold_left (fun s b' -> Sd.union s b'.scopeout) Sd.empty b.prev in
				let s' = (if b.typ = BFix then Sd.union s b.defs else Sd.diff s b.defs) in
				b.scopein <- chCmp b.scopein s');
			b.scopeout <- chCmp b.scopeout (Sd.union b.scopein b.defs)
		);
	done

let sdshow s = Sd.fold (fun(a,b) c -> (Symbol.name a ^ ":" ^ Symbol.name b)::c) s [] |> List.sort_uniq compare |> String.concat ","

let debug b = 
	let g = Vcg.create () in
	let rec doast parent t =
		let id = string_of_int t.meta.id in
		Vcg.node g id (Vcg.astname t) parent;
		Astutil.iter (fun t' ->
			Vcg.edge g id (string_of_int t'.meta.id)) t;
		id
	and doblock parent b =
		let name = Format.sprintf "G%d" b.bid in
		Vcg.node g name "" parent;
		Vcg.info g name (Format.sprintf "defs: %s\nscopein: %s\nscopeout: %s" (sdshow b.defs) (sdshow b.scopein) (sdshow b.scopeout));
		List.iter (fun x -> ignore (doast name x)) b.ast;
		List.iter (doblock name) b.sub;
		List.iter (fun x -> Vcg.edge g name (Format.sprintf "G%d" x.bid)) b.next;
		Vcg.color g name (match b.typ with
		| BSeq -> ""
		| BLet -> "lightred"
		| BFix -> "lightgreen")
	in doblock "" b;
	Vcg.pp Format.std_formatter g

let fixnames (p, pos, {block={scopein}; repl}) =
	match p with
	| Sym s ->
		(match repl with
		| Some x -> {p=Sym x; pos; meta=()}
		| None ->
			match Sd.fold (fun (s',t) r -> if s = s' then t::r else r) scopein [] with
			| [x] -> {p=Sym x; pos; meta=()}
			| [] -> Util.error pos ((Symbol.name s) ^ " undefined")
			| _ -> Util.error pos ((Symbol.name s) ^ " ambiguous"))
	| _ -> {p; pos; meta=()}

let analyse t =
	let b = newblock BFix None in
	let (t', _, _) = blockify b t in
	propagate b;
	debug b;
	Astutil.walk fixnames t'
