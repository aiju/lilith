module H = Hashtbl

module Type = struct
type typ =
	| Unit
	| Int
	| String
	| Bool
	| Type
	| Tuple of typ list
	| Array of typ
	| Fun of typ list * typ
	| TypeVar of Symbol.t
end

type binop =
	| OpAdd
	| OpSub
	| OpMul
	| OpDiv
	| OpMod
	| OpEq
	| OpNe
	| OpLt
	| OpLe
	| OpGt
	| OpGe

type unop =
	| OpUMinus
	| OpVar

type pos = Lexing.position * Lexing.position
type 'a ast_p =
	| Sym of Symbol.t
	| IntLit of int
	| TypeLit of Type.typ
	| Bin of binop * 'a ast * 'a ast
	| Un of unop * 'a ast
	| Assign of 'a ast * 'a ast
	| Call of 'a ast * 'a ast list
	| Seq of 'a ast list
	| Fix of 'a ast list
	| TypeIs of 'a ast * 'a ast option
	| If of 'a ast * 'a ast * 'a ast
	| While of 'a ast * 'a ast
	| DoWhile of 'a ast * 'a ast
	| For of 'a ast * 'a ast * 'a ast * 'a ast
	| Tuple of 'a ast list
	| Array of 'a ast list
	| Index of 'a ast * 'a ast list
	| Lambda of 'a ast * 'a ast
	| Let of 'a ast * 'a ast option
and 'a ast = {p: 'a ast_p; pos: pos; meta: 'a}
let posed pos p = {p;pos;meta=()}
let repos old p meta = {p; pos=old.pos; meta}
