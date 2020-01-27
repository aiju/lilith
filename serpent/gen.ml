module H = Hashtbl
open Dat

type state = {
	action: (Symbol.t, action) H.t;
	goto: (Symbol.t, int) H.t
}

let fn_header =
"exception ParseError

let parse lex buf =
	let gotostack = ref [] in
	let exprstack = ref [] in
	let peeked = ref None in
	let rec next () =
		match !peeked with
		| Some x -> peeked := None; x
		| None -> lex buf
	and peek () =
		match !peeked with
		| Some x -> x
		| None -> let s = lex buf in peeked := Some s; s
	and shift g s e =
		ignore (next ());
		exprstack := e :: !exprstack;
		gotostack := g :: !gotostack;
		s ()
	and push e = exprstack := e::!exprstack;
	and pop () = match !exprstack with
		| h::t -> exprstack := t; h
		| _ -> assert false
	and reduce n lhs =
		match n with
		| 0 -> (List.hd !gotostack) lhs
		| _ -> gotostack := List.tl !gotostack; reduce (n-1) lhs
	and accept () = Obj.obj(pop ())
	and error () = raise ParseError
"
let fn_footer =
"	in gotostack := [goto0]; state0 ()
"

let ntab = H.create 0
let nonterminal s =
	let rec trys s'' =
		match H.find_opt ntab s'' with
		| Some t when t = s -> s''
		| None ->
			H.add ntab s'' s;
			s''
		| _ -> trys (s''^"_")
	in
	let fixed = String.map (fun c ->
		match c with
		| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> c
		| _ -> '_') (Symbol.name s) in
	trys fixed

let printState f i st types restore_line =
	Format.fprintf f "\tand state%d () =@\n" i;
	Format.fprintf f "\t\tmatch peek () with@\n";
	H.iter (fun e a ->
		if H.mem types e then
			Format.fprintf f "\t\t| %a(_arg) -> " Symbol.pp e
		else
			Format.fprintf f "\t\t| %a -> let _arg = () in " Symbol.pp e;
		match a with
		| Shift(n) -> Format.fprintf f "shift goto%d state%d (Obj.repr(_arg))@\n" n n
		| Reduce(_,lhs,rhs,code) ->
			Format.fprintf f "@\n";
			let n = List.length rhs in
			List.rev rhs |> List.iteri (fun i x ->
				match H.find_opt types x with
				| Some t ->
					Format.fprintf f "\t\t\tlet _%d : %s = Obj.obj(pop ()) in\n" (n-i) t
				| _ -> 
					Format.fprintf f "\t\t\tignore (pop ());@\n"
			);
			Format.fprintf f "\t\t\tpush (Obj.repr (@\n";
			(match code with
			| None -> if n = 1 then Format.fprintf f "_1"
			| Some l -> List.iter (fun frag ->
					match frag with
					| Piece s -> Format.pp_print_string f s
					| Var n -> Format.fprintf f "_%d" n
					| Line (s,n) -> Format.fprintf f "# %d \"%s\"@\n" n s
				) l;
				Format.fprintf f "@\n";
				restore_line ());
			Format.fprintf f "\t\t\t));@\n";
			Format.fprintf f "\t\t\treduce %d NT_%s@\n" (List.length rhs) (nonterminal lhs);
		| Accept -> Format.fprintf f "accept ()@\n"
		| Error -> Format.fprintf f "error ()@\n"
	) st.action;
	Format.fprintf f "\t\t| _ -> error ()@\n";
	Format.fprintf f "\tand goto%d e =@\n" i;
	Format.fprintf f "\t\tmatch e with@\n";
	H.iter (fun e s' ->
		Format.fprintf f "\t\t| NT_%s -> gotostack := goto%d :: !gotostack; state%d ()\n" (nonterminal e) s' s') st.goto;
	Format.fprintf f "\t\t| _ -> assert false@\n"

let gen_ml name f lalr terminals types =
	let nlctr = ref 1 in
	(let fn = Format.pp_get_formatter_out_functions f () in
	let out s p n = String.iteri (fun i c ->
		if i >= p && i < p + n && c == '\n' then
			incr nlctr
	) s; fn.out_string s p n in
	Format.pp_set_formatter_out_functions f {fn with Format.out_string=out});
	let types = H.copy types in
	let nstates = H.fold (fun (id,_) _ c -> max c (id+1)) (LALR.actions lalr) 0 in
	let states = Array.init nstates (fun _ -> {action=H.create 0; goto=H.create 0}) in
	LALR.actions lalr |> H.iter (fun (id,e) a -> H.replace states.(id).action e a);
	LALR.goto lalr |> H.iter (fun (id,e) s' -> H.add states.(id).goto e s');
	
	LALR.nonterminals lalr |> List.iter (fun s ->
		if not (H.mem types s) then
			H.add types s ("'nt_" ^ (nonterminal s))
	);
	
	Format.fprintf f "type token = @\n";
	(LALR.endSym::terminals) |> List.iter (fun s ->
		match H.find_opt types s with
		| Some x -> Format.fprintf f "\t| %a of %s@\n" Symbol.pp s x
		| None -> Format.fprintf f "\t| %a@\n" Symbol.pp s
	);
	Format.fprintf f "type nonterminals = @\n";
	LALR.nonterminals lalr |> List.iter (fun s ->
		Format.fprintf f "\t| NT_%s@\n" (nonterminal s)
	);
	Format.fprintf f "%s" fn_header;
	Array.iteri (fun i st ->
		printState f i st types (fun () ->
			Format.fprintf f "# %d \"%s\"@\n" ((!nlctr) + 1) name
		)
	) states;
	Format.fprintf f "%s" fn_footer

let gen_mli f lalr terminals types =
	Format.fprintf f "type token = @\n";
	(LALR.endSym::terminals) |> List.iter (fun s ->
		match H.find_opt types s with
		| Some x -> Format.fprintf f "\t| %a of %s@\n" Symbol.pp s x
		| None -> Format.fprintf f "\t| %a@\n" Symbol.pp s
	);
	Format.fprintf f "@\nexception ParseError@\n";
	Format.fprintf f "@\nval parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> %s@\n" (H.find types (LALR.start lalr))
