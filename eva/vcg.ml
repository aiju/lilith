open Ast
module H = Hashtbl

type node = {
	title: string;
	mutable label: string;
	mutable info: string;
	mutable up: node
}
and t = {
	top: node;
	nodes: (string, node) H.t;
	mutable edges: (node * node) list
}

let create () = let rec top = {title=""; label=""; info=""; up=top} in {
	nodes= (let h = H.create 0 in H.add h "" top; h);
	edges= [];
	top
}
let rec findnode g title = 
	match H.find_opt g.nodes title with
	| Some n -> n
	| None ->
		let n = {title; label=""; info=""; up=g.top} in
		H.add g.nodes title n;
		n
let node g title label parent =
	let up = findnode g parent in
	let n = findnode g title in
	n.label <- label;
	n.up <- up
let edge g a b =
	let a' = findnode g a in
	let b' = findnode g b in
	g.edges <- (a',b')::g.edges
let info g n info = (findnode g n).info <- info

let rec pp f g =
	let sub = H.create 0 in
	H.add sub g.top [];
	H.iter (fun _ n -> if not (n==n.up) then Util.hmodify sub n.up [] (fun l -> n::l)) g.nodes;
	let rec donode n =
		match H.find_opt sub n with
		| None | Some [] when n != g.top -> 
			Format.fprintf f "@[node: {@[title:%S@ label:%S@ info1:%S@]}@]@ " n.title n.label n.info
		| None -> assert false
		| Some l ->
			Format.fprintf f "@[graph: {@[<v>title:%S@ info1:%S@ " n.title n.info;
			List.iter donode l;
			if n == g.top then (
				List.iter (fun (a, b)->
					Format.fprintf f "@[edge: {@[sourcename:%S@ targetname:%S@]}@ " a.title b.title
				) g.edges
			);
			Format.fprintf f "@]}@]@,"
	in donode g.top

let astname t =
	match t.p with
	| Sym x -> "Sym "^(Symbol.name x)
	| IntLit n -> "IntLit "^(string_of_int n)
	| Bin(o,a,b) -> let (s,_,_) = Astutil.opinfo o in ("Bin "^s)
	| Un(OpUMinus,a) -> "Un -"
	| Un(OpVar,a) -> "var"
	| Assign(a,b) -> "Assign"
	| Call(a,b) -> "Call"
	| Seq(l) -> "Seq"
	| Fix(l) -> "Fix"
	| TypeIs(a, Some b) -> "TypeIs"
	| TypeIs(a, None) -> "TypeIs"
	| If(a,b,c) -> "If"
	| While(a,b) -> "While"
	| DoWhile(a,b) -> "DoWhile"
	| For(a,b,c,d) -> "For"
	| Tuple(l) -> "Tuple"
	| Array(l) -> "Array"
	| Index(a,b) -> "Index"
	| Lambda(a,b) -> "Lambda"

let astshow f t =
	let g = create () in
	let idalloc = Util.idAllocator () in
	let rec walk t =
		let id = string_of_int (idalloc ()) in
		node g id (astname t) "";
		Astutil.iter (fun x -> edge g id (walk x)) t;
		id
	in ignore (walk t);
	pp f g
