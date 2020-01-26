type action = Shift of int | Goto of int | Reduce of int * Symbol.t * Symbol.t list | Accept | Error
type assoctype = Left | Right | Nonassoc
