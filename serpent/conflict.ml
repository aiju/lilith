module H = Hashtbl
open Dat

module Ss = Set.Make(Symbol)
type state = {
	id: int;
	actions: (Symbol.t, action) H.t
}
type graph = {
	states: state array;
	mutable terminals: Ss.t
}
type node = {
	stack: int list;
	mutable input: Symbol.t list;
	mutable cost: int;
	mutable idx: int
}
type heap = node array ref
let heapswap h i j =
	let t = h.(i) in
	h.(i) <- h.(j);
	h.(j) <- t;
	h.(i).idx <- i;
	h.(j).idx <- j
let rec heapup h i =
	let j = (i-1)/2 in
	if i > 0 && h.(i).cost < h.(j).cost then (
		heapswap h i j;
		heapup h j
	)
let rec heapdown h i =
	let left = 2 * i + 1 and right = 2 * i + 2 and n = Array.length h in
	let best = if left < n && h.(left).cost < h.(i).cost then left else i in
	let best = if right < n && h.(right).cost < h.(best).cost then right else best in
	if best <> i then (
		heapswap h i best;
		heapdown h best
	)
let heapadd h n =
	assert(n.idx < 0);
	n.idx <- Array.length !h;
	h := Array.append !h [|n|];
	heapup !h n.idx
let heapdel h n =
	assert(n.idx >= 0);
	let j = Array.length !h - 1 in
	let k = n.idx in
	heapswap !h k j;
	h := Array.sub !h 0 j;
	heapdown !h k;
	n.idx <- -1
let heappop h =
	let n = (!h).(0) in
	heapdel h n;
	n

let dijkstra g target =
	let h = ref [||] in
	let nodes = H.create 0 in
	let newNode stack input cost =
		let n = {stack; cost; input; idx= -1} in
		H.add nodes stack n;
		heapadd h n
	in let upNode stack input cost =
		match H.find_opt nodes stack with
		| None -> newNode stack input cost
		| Some v ->
			if cost < v.cost then (
				v.cost <- cost;
				v.input <- input;
				heapup !h v.idx;
			)
	in newNode [0] [] 0;
	while Array.length (!h) > 0 && List.hd (!h).(0).stack <> target do
		let u = heappop h in
		let s = g.states.(List.hd u.stack) in
		H.iter (fun e a ->
			match a with
			| Shift(p') ->
				let s' = g.states.(p') in
				upNode (s'.id::u.stack) (e::u.input) (u.cost + 1)
			| Reduce(lhs,rhs) ->
				let stack' = ref u.stack in
				List.iter (fun _ -> stack' := List.tl !stack') rhs;
				let s' = g.states.(List.hd !stack') in
				let s'' = (match H.find s'.actions lhs with
					| Goto(p') -> g.states.(p')
					| _ -> assert false) in
				upNode (s''.id::!stack') u.input u.cost
			| _ -> ()
		) s.actions
	done;
	if Array.length (!h) > 0 then
		List.iter (Format.printf "%a " Symbol.pp) (List.rev (!h).(0).input)

let print actions (p,a) =
	let nstates = H.fold (fun (p,_) _ c -> max (p+1) c) actions 0 in
	let g = {
		states=Array.init nstates (fun id -> {id;actions=H.create 0});
		terminals=Ss.empty
	} in
	H.iter (fun (p,e) a ->
		H.add g.states.(p).actions e a
	) actions;
	dijkstra g p

