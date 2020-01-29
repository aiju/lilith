open Ast
open Util

module Ss = Set.Make(Symbol)
type meta = {
	scope: Ss.t
}

let metaShow m =
	Ss.fold (fun b a -> a^" "^(Symbol.name b)) m.scope ""

let union l = List.fold_left (fun a b -> Ss.union a b) Ss.empty l
let merge l = List.fold_left (fun a b -> Ss.union a b.meta.scope) Ss.empty l
let addDefs d l = union [l;d]

let rec patDescend scope t =
	match t.p with
	| Sym s -> (repos t (Sym s) {scope}, Ss.add s Ss.empty)
	| Tuple l ->
		let (l', defs) = List.fold_left (fun (l, defs) c ->
			let (c', defs') = patDescend scope c in
			(c'::l, union[defs;defs'])) ([], Ss.empty) l in
		(repos t (Tuple(List.rev l')) {scope=merge l'}, defs)
	| _ -> error t.pos "invalid pattern"
and lvalDescend scope t =
	match t.p with
	| Sym s -> (repos t (Sym s) {scope}, Ss.empty)
	| TypeIs(a,b) ->
		let (a', defs) = patDescend scope a in
		let b' = optionMap (descend scope) b in
		(repos t (TypeIs(a',b')) {scope=merge ([a']@(optionToList b'))}, defs)
	| Tuple(l) ->
		let (l', defs) = List.fold_left (fun (l, defs) c ->
			let (c', defs') = lvalDescend scope c in
			(c'::l, union[defs;defs'])) ([], Ss.empty) l in
		(repos t (Tuple(List.rev l')) {scope=merge l'}, defs)
	| _ -> error t.pos "invalid lval"
and descend scope (t:unit Ast.ast) =
	match t.p with
	| Fix(l) ->
		let l' = List.map (descend scope) l in
		let l'' = List.map (descend (merge l')) l in
		repos t (Fix(l'')) {scope=merge l''}
	| Seq(l) ->
		let (scope', l') = List.fold_left (fun (scope,l) t ->
			let t' = descend scope t in
			(t'.meta.scope, t'::l)) (scope,[]) l in
		repos t (Seq(List.rev l')) {scope=scope'}
	| Sym s -> repos t (Sym s) {scope}
	| IntLit n -> repos t (IntLit n) {scope}
	| Bin (o, a, b) ->
		let a' = descend scope a and
		b' = descend scope b in
		repos t (Bin(o, a', b')) {scope=merge [a';b']}
	| Un (o, a) ->
		let a' = descend scope a in
		repos t (Un(o, a')) {scope=a'.meta.scope}
	| Tuple l ->
		let l' = List.map (descend scope) l in
		repos t (Tuple(l')) {scope=merge l'}
	| Array l ->
		let l' = List.map (descend scope) l in
		repos t (Array(l')) {scope=merge l'}
	| If(a,b,c) ->
		let a' = descend scope a in
		let b' = descend a'.meta.scope b in
		let c' = descend a'.meta.scope c in
		repos t (If(a',b',c')) {scope=merge [b';c']}
	| While(a,b) ->
		let a' = descend scope a in
		let b' = descend a'.meta.scope b in
		repos t (While(a',b')) {scope=a'.meta.scope}
	| DoWhile(a,b) ->
		let a' = descend scope a in
		let b' = descend a'.meta.scope b in
		repos t (DoWhile(a',b')) {scope=b'.meta.scope}
	| For(a,b,c,d) ->
		let a' = descend scope a in
		let b' = descend a'.meta.scope b in
		let d' = descend b'.meta.scope d in
		let c' = descend d'.meta.scope c in
		repos t (For(a',b',c',d')) {scope=b'.meta.scope}
	| Index(a,b) ->
		let a' = descend scope a in
		let b' = List.map (descend scope) b in
		repos t (Index(a',b')) {scope=merge (a'::b')}
	| Call(a,b) ->
		let a' = descend scope a in
		let b' = List.map (descend scope) b in
		repos t (Call(a',b')) {scope=merge (a'::b')}
	| TypeIs(a,Some b) ->
		let a' = descend scope a in
		let b' = descend scope b in
		repos t (TypeIs(a', Some b')) {scope=merge [a';b']}
	| TypeIs(a,None) ->
		let a' = descend scope a in
		repos t (TypeIs(a', None)) {scope=a'.meta.scope}
	| Assign(a, b) ->
		let (a', defs) = lvalDescend scope a in
		let b' = descend scope b in
		let scope' = addDefs defs (merge[a';b']) in
		repos t (Assign(a', b')) {scope=scope'}
	| Lambda(a, b) ->
		let (a', defs) = lvalDescend scope a in
		let b' = descend (addDefs defs scope) b in
		repos t (Lambda(a',b')) {scope}

let analyse t = 
	descend Ss.empty t
