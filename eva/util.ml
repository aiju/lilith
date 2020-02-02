module H = Hashtbl

let locShow ((p1:Lexing.position),(p2:Lexing.position)) =
	Format.sprintf "%s:%d-%d:%d-%d" p1.pos_fname p1.pos_lnum p2.pos_lnum p1.pos_cnum p2.pos_cnum

let error loc msg =
	Format.fprintf (Format.err_formatter) "%s %s\n%!" (locShow loc) msg;
	exit 1

let optionMap f x =
	match x with
	| Some x' -> Some (f x')
	| None -> None

let optionToList x =
	match x with
	| Some x -> [x]
	| None -> []

let optionFold f y x =
	match x with
	| Some x' -> f x' y
	| None -> y

let idAllocator () =
	let id = ref 0 in
	fun () -> let n = !id in incr id; n

let hkeys t =
	let l = ref [] in
	H.iter (fun x _ -> l := x::!l) t;
	List.sort_uniq compare !l

let hmodify t k def f =
	match H.find_opt t k with
	| None -> H.replace t k (f def)
	| Some x -> H.replace t k (f x)

let rec chop n l =
	match n, l with
	| 0, _ -> l
	| _, [] -> []
	| n, _::t -> chop n t

let chopTail n l = List.rev (chop n (List.rev l))

let rec last l =
	match l with
	| [] -> raise (Failure "last")
	| [x] -> x
	| _::t -> last t

let rec iterAdj f l =
	match l with
	| h::((h'::_) as t) -> f h h'; iterAdj f t
	| _ -> ()
