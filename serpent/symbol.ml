type t = int

module H = Hashtbl

let nametab : string array ref = ref [||]
let idtab : (string,t) H.t = H.create 0

let get s = match H.find_opt idtab s with
	| Some x -> x
	| None ->
		let n = Array.length !nametab in
		nametab := Array.append !nametab [|s|];
		H.add idtab s n;
		n

let temp s =
	let rec f n =
		let s' = s ^ (string_of_int n) in
		if H.mem idtab s' then
			f (n+1)
		else
			get s'
	in f 0

let name id = (!nametab).(id)
let pp f n = Format.fprintf f "%s" (name n)
let compare = compare
