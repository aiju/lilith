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
