type t

val get : string -> t
val name : t -> string
val id : t -> int
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val temp : string -> t
