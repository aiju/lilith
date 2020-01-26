type ('a, 'b) t

(* note that the function must be monotonic *)
val create : ('b -> 'b -> 'b) -> ('a,'b) t
val node : ('a,'b) t -> 'a -> 'b -> unit
val edge : ('a,'b) t -> 'a -> 'a -> unit
val eval : ('a,'b) t -> ('a,'b) Hashtbl.t
val iterEdge : ('a,'b) t -> ('a -> 'a -> unit) -> unit

