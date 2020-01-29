type t

val get : string -> t
val name : t -> string
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
