type t

type rule = {
	lhs: Symbol.t;
	rhs: Symbol.t list;
	prec: (int * Dat.assoctype) option;
	idx: int;
	code: Dat.fragment list option
}

val startSym : Symbol.t
val endSym : Symbol.t
val create : rule list -> (Symbol.t, int * Dat.assoctype) Hashtbl.t -> Symbol.t -> t

val actions : t -> (int * Symbol.t, Dat.action) Hashtbl.t
val goto : t -> (int * Symbol.t, int) Hashtbl.t
val terminals : t -> Symbol.t list
val nonterminals : t -> Symbol.t list
val start : t -> Symbol.t
val rules : t -> (Symbol.t, rule) Hashtbl.t

val printStates : Format.formatter -> t -> unit
val findconflicts : t -> (int, Symbol.t * int * int) Hashtbl.t * (int, Symbol.t * int * int) Hashtbl.t

