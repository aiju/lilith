# The Eva programming language
## Expressions and statements

Expressions in Eva follow mostly standard C-like syntax

```
	a + b * (c - d)
	f(b,d)
```

Control structure exist in both a C-like statement form and an expression form. (Note the missing semicolon in the second case)

```
	if(x) y; else z;
	k = if(x) y else z;
```

Unlike C, blocks are written using parentheses:

```
	if(x) (
		foo();
		bar();
	)
```

In Eva, statements are simply expressions that return `()` (unit), although as we have already seen the placement of semicolons sometimes differs.
Blocks are permitted everywhere expressions are permitted:

```
	(foo(); bar(); 42)
	(foo(); bar(); )
```

The first example has the value 42, the second example has the value `()`.

Anonymous or lambda functions use the syntax
```
	x -> x + 1
	x, y -> x + y
```

## Assignments and definitions

Assignments take the form `lhs = rhs` and have the value `()`. The `lhs` can be a pattern:

```
	x = y;
	_ = y;
	x, y = 42, 23;
```

New names can be defined using a pattern of the form `name:type` (where `type` is optional):

```
	x : int = 42;
	x : = 23;
```

Note that the second form is often written as `x := 23`, but care should be taken with tuples:

```
	x, y: = 42, 23;
```

This reassigns x and redefines y.

Functions are defined using the syntax

```
	foo(x, y) := x + y;
```

Definitions can be recursive and adjacent definitions can be mutually recursive:

```
	gcd(x, y) :=
		if(y != 0) gcd(y, x % y)
		else x;
	
	a() := b();
	b() := a();
```

## Scoping

Eva uses an unusual scoping scheme which I call "control-flow scoping". The basic rules are

1. A definition of `x` at point A is visible at point B if all paths from A lead to B without a redefinition of `x`.
2. `x` is legal to use if only one definition of `x` is visible.
3. A path is determined statically and every branch is assumed to be able to go either way.
4. Evaluation order within expression is undefined and all possible orders must be considered.
5. Functions use variables at the point of definition and not at the point of use.

The following examples are all legal:

```
	x := 42;
	if(a)
		x;
	x
```
```
	if(a) (
		x := 42;
		x;
	)
```
```
	if(x := 42; a)
		x;
	x
```
```
	(x := 42; x) + 23
```
```
	f := if(a) (
		x := 42;
		() -> x
	) else ...;
	f() /* returns 42 if a is true */
```

The following are not:

```
	if(a)
		x := 42;
	x
```
```
	if(a)
		x := 42;
	else
		x := 23;
	x
```
```
	f := () -> x;
	x := 42;
	f()
```
```
	(x := 42; x) + x;
```

The non-ambiguity rule 2 might be changed in the future to:

	2. `x` is legal to use if it is defined on all paths. The type of `x` is the union of the types of all definitions.
