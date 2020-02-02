module H = Hashtbl

module Int = struct type t = int let compare = compare end
module Si = Set.Make(Int)

type ('a, 'b) node = {
	id: int;
	key: 'a;
	mutable value: 'b;
	mutable next: Si.t;
	mutable prev: Si.t
}
type ('a, 'b) t = {
	nodes: ('a, ('a, 'b) node) H.t;
	default: 'a -> 'b;
	idalloc: unit -> int
}

let create default =
	{nodes=H.create 0; default; idalloc=Util.idAllocator()}

let mknode g key value =
	match H.find_opt g.nodes key with
	| Some n -> n.value <- value; n
	| None ->
		let n = {key; value; next=Si.empty; prev=Si.empty; id=g.idalloc()} in
		H.add g.nodes key n;
		n

let node g key value = ignore (mknode g key value)

let getnode g key =
	match H.find_opt g.nodes key with
	| Some n -> n
	| None -> mknode g key (g.default key)

let edge g k1 k2 =
	let n1 = getnode g k1 and n2 = getnode g k2 in
	n1.next <- Si.add n2.id n1.next;
	n2.next <- Si.add n1.id n2.next

