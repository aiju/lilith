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

## Control structures

Our good friends from C:
```
if(...) ...;
if(...) ... else ...;
while(...) ...;
do ... while(...);
for(...; ...; ...) ...;
break;
continue;
goto ...;
return ...;
```

The labeled `break` and `continue` statements from more recent languages are also supported.

Labels are defined using

```
=> loop =>
	goto loop;
```

Instead of `switch`, there is `case`, which does pattern matching

```
case((x,y)) (
(23,42) => ...
(42,23) => ...
_ => ...
)
```

Cases can also be made to fallthrough using the syntax:

```
=> (42,23) => ...
```

## Scoping

Eva uses an unusual scoping scheme which I call "control-flow scoping". The basic rules are

1. A definition of `x` at point A is visible at point B if all paths from A lead to B without a redefinition of `x`.
2. `x` is legal to use if only one definition of `x` is visible.
3. Paths are determined statically and every branch is assumed to be able to go either way.
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

### Fix blocks

In a `fix` blocks all names are visible within all other names in the block and in whatever follows the block:

```
fix (
a := () -> b();
b := () -> a();
);
```

Adjacent function definitions using `foo(...) := ...` syntax are automatically embedded in implicit `fix` blocks.

### Let

`let` can be used to prevent a variable from escaping the current block:

```
(let x := 42; ...);
x /* illegal */
```

`let foo in bar` is syntactic sugar for `(let foo; bar)`.

## Functions

I already mentioned the syntax
```
foo(a, b) := a+b
```

If names are preceded by `@` then the arguments can be named in calls:
```
foo(@a, @b) := a+b;
foo(a=42, b=23);
```

Defaults can be given as well:
```
foo(@a=42, @b=23) := a+b;
foo(b=1);
```

Function signatures can be specified using the syntax
```foo : (@a : int = 42, @b : int = 23) -> int = ?;```

## Types

### Primitive types

- `int` = `int32`
- `byte` = `uint8`
- `int8`, `int16`, `int32`, `int64`
- `uint8`, `uint16`, `uint32`, `uint64`

- `string` (UTF-8 strings)
- `rune` (Unicode codepoint)
- `bool`
- `float` = `float64`
- `float32`, `float64`

### Composite types

- Tuples, e.g. `(string, int, float)`:
Tuples can be pattern matched or their components accessed using `.0`, `.1`, etc. syntax.

- Arrays, e.g. `[string]` or `[int]` (arbitrary length):
Arrays can be indexed using `foo[42]` syntax.

### Reference types

An explicit reference (or pointer) type is defined using the syntax `ref type`. Explicit references behave like C pointers without pointer arithmetic.

By contrast, an implicit reference type (syntax `&type`) is automatically dereferenced whereever it appears.

`var <type>` returns an implicit reference to new memory of the given type. It's the preferred way of obtaining a mutable variable:

```
i := var 0;
i = i + 1;
```

### Type aliases

A type alias is defined using

```
newname := type oldname;
```

### Algebraic data types

Simple enumerations are defined using the syntax

```
enum (Red, Green, Blue)
```

Each value can also be given arguments just like a function:

```
enum (RGB(@R: int, @G: int, @B: int), HSV(@H: int, @S: int, @V: int))
```

Recursive types can be defined using type aliases:

```
list := type enum (Nil, List(item, list));
```

Using the values as constructors or patterns is straightforward:

```
length(Nil) := 0;
length(Cons(_, rest)) := 1 + length(rest);
```

### Generic types

Generic types are defined using

```
list(t) := type enum (Nil, List(t, list(t)));
```

The type signature of `length` from the last section is then

```
length : gen(t) (list(t) -> int) = ?;
```

### Traits

A trait specifies an abstract interface that can be satisfied by a type:

```
Show(t) := trait (
	show : t -> string = ?;
);
Number(t) := trait (
	(+) : t -> t -> t = ?;
	(-) : t -> t -> t = ?;
	(*) : t -> t -> t = ?;
	(/) : t -> t -> t = ?;
	(%) : t -> t -> t = ?;
);
```

To implement a trait the syntax is

```
Show(Color).show t := case(t) (
	Red => "Red"
	Green => "Green"
	Blue => "Blue"
);
```

# Maybe Features
## Slices
A slice allows a section of an array to be referenced.
Potentially multidimensional.

## Goto Parameters
There is an alternative kind of label which doesn't fall through:

```
/* implicit return to above block here */
loop =>
	goto loop;
```

Labels can be given parameters, which are assigned to local variables.
For this to make sense, the parameters must have either default values, been valid before the label or the code before the label must be dead.

```
a := 42;
=> lab(a, b=23) =>
```

Using a label in a non-goto context promotes it to a closure:

```
f := lab; /* equivalent to: */
f := (a, b=23) -> goto lab(a, b);
```

Pattern matching on goto cases is also supported:

```
lab(23) =>
	...
lab(42) =>
	...
lab(_) =>
	...
	
	goto lab(i);
```
