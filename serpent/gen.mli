val gen_ml : string -> Format.formatter -> LALR.t -> Symbol.t list -> (Symbol.t, string) Hashtbl.t -> unit
val gen_mli : Format.formatter -> LALR.t -> Symbol.t list -> (Symbol.t, string) Hashtbl.t ->unit
