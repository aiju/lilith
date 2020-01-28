module H = Hashtbl
open Dat

type state = {
	action: (Symbol.t, action) H.t;
	goto: (Symbol.t, int) H.t
}

let fn_header =
"exception ParseError

type range = Lexing.position * Lexing.position
let merge (p1,p2) (p1',p2') = (p1,p2')
type state = {
	fn: unit -> unit;
	goto: nonterminal -> state;
	follow: string list
}
type stackel = {
	state: state;
	expr: Obj.t;
	pos: range
}

let parse lex buf errorfn =
	let stack = ref [] in
	let peeked = ref None in
	let rec next () =
		match !peeked with
		| Some x -> peeked := None; x
		| None -> lex buf
	and peek () =
		match !peeked with
		| Some x -> x
		| None -> let s = lex buf in peeked := Some s; s
	and accept () = match !stack with {expr}::_ -> Obj.obj(expr) | _ -> assert false
	and error tok follow =
		(if follow = [] then
			Format.sprintf \"syntax error, got %s\" (token_show tok)
		else
			(Format.sprintf \"syntax error, got %s, expected one of \" (token_show tok)) ^
			(List.fold_left (fun a b -> a ^ \" \" ^ b) (List.hd follow) (List.tl follow))
		) |> errorfn (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf);
		assert false
"
let fn_footer =
"	in stack := [{state=state0; expr=Obj.repr(()); pos=(Lexing.dummy_pos, Lexing.dummy_pos)}]; statefn0 ()
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
	Format.fprintf f "\tand statefn%d () =@\n" i;
	Format.fprintf f "\t\tmatch peek () with@\n";
	H.iter (fun e a ->
		if H.mem types e then
			Format.fprintf f "\t\t| %a(_arg) -> " Symbol.pp e
		else
			Format.fprintf f "\t\t| %a -> let _arg = () in " Symbol.pp e;
		match a with
		| Shift(n) ->
			Format.fprintf f "@\n\t\t\tlet pos = (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf) in@\n";
			Format.fprintf f "\t\t\tlet state' = {state=state%d; expr=(Obj.repr(_arg)); pos} in@\n" n;
			Format.fprintf f "\t\t\t\tignore (next ()); stack := state' :: !stack; statefn%d ()\n" n
		| Reduce(_,lhs,rhs,code) ->
			Format.fprintf f "(match !stack with@\n\t\t\t";
			let n = List.length rhs in
			List.rev rhs |> List.iteri (fun i x ->
				Format.fprintf f "_S%d::" (n-i)
			);
			Format.fprintf f "({state={goto}}::_ as rest) ->@\n";
			List.rev rhs |> List.iteri (fun i x ->
				match H.find_opt types x with
				| Some t ->
					Format.fprintf f "\t\t\t\tlet _%d : %s = Obj.obj _S%d.expr in@\n" (n-i) t (n-i)
				| _ ->  ()
			);
			Format.fprintf f "\t\t\t\tlet expr = Obj.repr (";
			(match code with
			| None -> if n = 1 then Format.fprintf f "_1"
			| Some l -> List.iter (fun frag ->
					match frag with
					| Piece s -> Format.pp_print_string f s
					| Var n -> Format.fprintf f "_%d" n
					| Line (s,n) -> Format.fprintf f "@\n# %d \"%s\"@\n" n s
				) l;
				Format.fprintf f "@\n";
				restore_line ());
			if n <> 0 then
				Format.fprintf f "\t\t\t\t) in let pos = merge _S1.pos _S%d.pos@\n" n
			else
				Format.fprintf f "\t\t\t\t) in let pos = (Lexing.lexeme_end_p buf, Lexing.lexeme_end_p buf)@\n";				
			Format.fprintf f "\t\t\t\tin let state' = goto NT_%s in@\n" (nonterminal lhs);
			Format.fprintf f "\t\t\t\tstack := {state=state'; expr; pos}::rest; state'.fn ()@\n";
			Format.fprintf f "\t\t\t| _ -> assert false)@\n"
		| Accept -> Format.fprintf f "accept ()@\n"
		| Error -> Format.fprintf f "error (peek()) state%d.follow@\n" i
	) st.action;
	Format.fprintf f "| _ -> error (peek()) state%d.follow@\n" i;
	Format.fprintf f "\tand goto%d nt =@\n" i;
	Format.fprintf f "\t\tmatch nt with@\n";
	H.iter (fun e s' ->
		Format.fprintf f "\t\t| NT_%s -> state%d\n" (nonterminal e) s') st.goto;
	Format.fprintf f "\t\t| _ -> assert false@\n";
	Format.fprintf f "\tand state%d = {fn=statefn%d; goto=goto%d; follow=List.sort_uniq compare [" i i i;
	H.iter (fun e a ->
		match a with
		| Error -> ()
		| _ -> Format.fprintf f "\"%a\";" Symbol.pp e
	) st.action;
	Format.fprintf f "]}@\n"

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
	Format.fprintf f "let token_show t = match t with\n";
	(LALR.endSym::terminals) |> List.iter (fun s ->
		match H.find_opt types s with
		| Some x -> Format.fprintf f "\t| %a(_) -> \"%a\"@\n" Symbol.pp s Symbol.pp s
		| None -> Format.fprintf f "\t| %a -> \"%a\"@\n" Symbol.pp s Symbol.pp s
	);
	
	Format.fprintf f "type nonterminal = @\n";
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
	Format.fprintf f "@\nval parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position -> string -> unit) -> %s@\n" (H.find types (LALR.start lalr))
