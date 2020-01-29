let _ =
	let lexbuf = Lexing.from_channel stdin in
	let result = Parse_gen.parse Lexer.token lexbuf Util.error in
		Ast.vcgShow Format.std_formatter Semant.metaShow (Semant.analyse result)
