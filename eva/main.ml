let _ =
	let lexbuf = Lexing.from_channel stdin in
	let result = Parse_gen.parse Lexer.token lexbuf Util.error in
		Vcg.astshow Format.std_formatter (Scope.analyse result)
