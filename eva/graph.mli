type ('a,'b) t

val create : ('a -> 'b) -> ('a, 'b) t
val node : ('a, 'b) t -> 'a -> 'b -> unit
val edge : ('a, 'b) t -> 'a -> 'a -> unit
