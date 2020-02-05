let _ =
	let lexbuf = Lexing.from_channel stdin in
	Parse_gen.parse Lexer.token lexbuf Util.error
	|> Scope.analyse
	|> Types.analyse

		
