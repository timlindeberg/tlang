# Operator Overloading
Operator overloading is supported in `tlang` where the following operators 
can be extended:

### Binary Operators
```tlang
+ - * / % & | ^ << >> < <= > >= == !=
```

### Unary Operators
```tlang
~ # ++ --
```

### Indexing Operators
```tlang
[] []= [:]
```

Operators are implicitly `static` and you have to declare all arguments to.
The arguments can be of any type but at least one of them has to of the same type as the class
their declared in.

Operators are not commutative, that is, the order of the operands matter so the following operators
are not the same:

```tlang
class A =
	Def +(lhs: A, rhs: String) = "ABC"
	Def +(lhs: String, rhs: A) = "123"

new A() + "BC" // ABC
"BC" + new A() // 123
```

Operators can be defined in both classes, traits and extension methods, however, an operator cannot
be abstract.

Example:

```tlang
class A =

	// Binary operators  
	Def +(lhs: A, rhs: A): A = ...
	Def -(lhs: A, rhs: A): A = ...
	Def *(lhs: A, rhs: A): A = ...
	Def /(lhs: A, rhs: A): A = ...
	Def %(lhs: A, rhs: A): A = ...
	Def &(lhs: A, rhs: A): A = ...
	Def |(lhs: A, rhs: A): A = ...
	Def ^(lhs: A, rhs: A): A = ...
	Def <<(lhs: A, rhs: A): A = ...
	Def >>(lhs: A, rhs: A): A = ...

	Def <(lhs: A, rhs: A): Bool = ...
	Def <=(lhs: A, rhs: A): Bool = ...
	Def >(lhs: A, rhs: A): Bool = ...
	Def >=(lhs: A, rhs: A): Bool = ...
	Def ==(lhs: A, rhs: A): Bool = ...
	Def !=(lhs: A, rhs: A): Bool = ...

	// Unary operators
	Def ~(a: A): A = ...
	Def #(a: A): Int = ...
	Def ++(a: A): A = ...
	Def --(a: A): A = ...

	// Indexing operators
	Def [](a: A, index: Int): A = ...
	Def []=(a: A, index: Int, value: String): Unit = ...
	Def [:](a: A, start: Int?, end: Int?, step: Int?): A = ...


val a1 = new A()
val a2 = new A()

a1 + a2
a1 == a2

#a
a[6]
a[123] = "ABC"
a[1:2:3]
```

The return type of an operator can generally be anything with a few exceptions:

- Comparison operators, `<, <=, >, >=, ==, !=`, must return `Bool`
- The Hash operator, `#`, must return `Int`
- The Index assignment operator, `[]=`, must return `Unit`

## Indexing Operators
Most operators accept a given number of operands of any type but there are some special cases:

### Indexing
The indexing operator, `[]` accepts the type to index as the first argument and an index as it's second
argument.

```tlang
class A =
	Def [](a: A, index: Int) = ...
	Def [](a: A, index: String) = ...
	Def [](a: A, index: A) = ...

val a = new A()

a[5]
a["ABC"]
a[new A()]
```

### Index assignment
Similarly, the index assignment operator accepts the type to index, an index of any type and a value of any type.

```tlang
class A =
	Def []=(a: A, index: Int, value: Int): Unit = ...
	Def []=(a: A, index: String, value: Long): Unit = ...
	Def []=(a: A, index: A, value: String): Unit = ...

val a = new A()

a[5] = 5
a["ABC"] = 8L
a[new A()] = "ABC"
```

The return type of the operator should always be Unit but the result of the expression is the right hand operand which
allows the chaining of assignments.

### Slice
The slice operator accepts the type to slice, a `start` index, an `end` index and a `step` size.
The types of these variables can be anything but are always nullable. If one of the values is
missing from the expression the variable will be `null`.

```tlang
class A =
	Def [:](a: A, start: Int?, end: String?, step: Char?): A = 
		println(start + " " + end + " " + step)
		a

val a = new A()

a[:] // null null null
a[1:] // 1 null null
a[:"ABC"] // null ABC null
a[::'A'] // null null A
a[5:"ABC":'D'] // 5 ABC D
```

## Assignment operators
If an operator is declared the corresponding assignment operator is implicitly defined.

```tlang
class A =

	var s: String

	Def new(s: String) = (this.s = s)

	Def +(a: A, s: String) = new A(a.s + s)

	Def ToString() = s

var a = new A("ABC")
val x = a
a += "123"

println(x) // ABC
println(a) // ABC123
``` 

The statement `a += "123"` is translated by the compiler to `a = a + "123"`. In the future it
will be possible to customize the behaviour of the assignment operators.

Overloading the increment and decrement operators will overload both the pre- and post operators.
The operator can modify the object and should typically return a reference to the object modified,
but can return anything.

