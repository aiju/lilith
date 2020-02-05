open Ast
open Type

let vars = List.map (fun (a,b,c) -> (Symbol.get a, b, c)) [
	"int", TypeLit Int, Type;
	"bool", TypeLit Bool, Type;
	"string", TypeLit String, Type
]

let tab : (Symbol.t, unit ast_p * typ) H.t =
	let h = H.create 0 in
	List.iter (fun (a,b,c) -> H.add h a (b,c)) vars;
	h
