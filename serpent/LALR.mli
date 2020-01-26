type t

type rule = {
	lhs: Symbol.t;
	rhs: Symbol.t list;
	prec: (int * Dat.assoctype) option;
	idx: int;
	action: string option
}

val startSym : Symbol.t
val endSym : Symbol.t
val create : rule list -> (Symbol.t, int * Dat.assoctype) Hashtbl.t -> Symbol.t -> t
val printStates : Format.formatter -> t -> unit
