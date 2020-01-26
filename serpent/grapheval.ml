type ('a,'b) node = {
	id: int;
	tag: 'a;
	mutable succ: ('a,'b) node list;
	mutable pred: ('a,'b) node list;
	mutable value: 'b;
	mutable index: int;
	mutable lowlink: int;
	mutable onStack: bool
}
let nodecmp n m = compare n.id m.id
module H = Hashtbl
type ('a,'b) t = {
	nodes: ('a, ('a, 'b) node) H.t;
	eval: 'b -> 'b -> 'b;
	mutable idctr: int
}
let create fn = {
	nodes=H.create 0;
	eval=fn;
	idctr=0
}
let node g a b = 
	match H.find_opt g.nodes a with
	| None ->
		let n = {
			tag=a;
			succ=[];
			pred=[];
			id=g.idctr;
			value=b;
			index= -1;
			lowlink= -1;
			onStack=false
		} in
		g.idctr <- g.idctr + 1;
		H.add g.nodes a n
	| Some n -> n.value <- b
let edge g a b =
	let a' = H.find g.nodes a and b' = H.find g.nodes b in
	if not (List.memq b' a'.succ) then (
		a'.succ <- b'::a'.succ;
		b'.pred <- a'::b'.pred
	)

let tarjan g =
	let index = ref 0
	and stack = ref [] 
	and res = ref [] in
	let rec strongconnect n =
		n.index <- !index;
		n.lowlink <- !index;
		index := !index + 1;
		stack := n::!stack;
		n.onStack <- true;
		List.iter (fun m ->
			if m.index < 0 then (
				strongconnect m;
				n.lowlink <- min n.lowlink m.lowlink
			) else if m.onStack then
				n.lowlink <- min n.lowlink m.index
		) n.succ;
		if n.lowlink = n.index then
			let rec f () =
				let m = List.hd !stack in
				stack := List.tl !stack;
				m.onStack <- false;
				m::(if m == n then [] else f ())
			in res := (f ()) :: !res
	in H.iter (fun _ n ->
		if n.index < 0 then
			strongconnect n
	) g.nodes;
	!res

let eval g = 
	let scc = tarjan g and result = H.create (H.length g.nodes) in
	List.iter (fun c ->
		let v = List.fold_left (fun n m -> g.eval n m.value) (List.hd c).value (List.tl c) in
		let pred = List.sort_uniq (fun a b -> compare a.index b.index)
			(List.concat (List.map (fun n -> n.pred) c)) in
		let v' = List.fold_left (fun n m -> g.eval n m.value) v pred in
		List.iter (fun n -> n.value <- v'; H.replace result n.tag v') c
	) scc;
	result

let iterEdge g f =
	g.nodes |> H.iter (fun a n ->
		n.succ |> List.iter (fun {tag=b} ->
			f a b
		)
	)
