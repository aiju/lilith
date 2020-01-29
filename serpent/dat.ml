type fragment = Piece of string | Var of int | Line of string * int
type action =
	| Shift of int
	| Reduce of int * Symbol.t * Symbol.t list * fragment list option
	| Accept
	| Error
type assoctype = Left | Right | Nonassoc | Unary
