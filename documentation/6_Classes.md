# Classes
`tlang` is an object oriented language and support your usual object oriented style classes and inheritance.
A class can contain fields, constructors, methods and operators.

```tlang
class A =

	var a: Int
	val b: String

	Def new(a: Int, b: String) = 
		this.a = a;
		this.b = b;

	Def Print(): Unit = println(b + " " + a)
```

A constructor in `tlang` is defined using the `new` keyword. Class `A` can now be instantiated and used like this:

```tlang
val a = new A(123, "ABC")
a.Print() // ABC 123
``` 

## Inheritance
A class can extend another class:

```tlang
class B =
	Def X(): Unit = println("B")

class A : B =
	Def X(): Unit =
		super.X()
		println("A")

new A().X() // prints B then A

```

## Traits
Traits are classes with abstract methods and are defined using the `trait` keyword. 
A class may extend from multiple traits. Traits may also contain non-abstract methods.

```tlang
trait A =
	Def X(): Int
	Def Z() = 3

trait B =
	Def Y(): Int

class C : A, B =
	Def X(): Int = 1
	Def Y(): Int = 2

	Def Calculate() = X() + Y() + Z()

new C().Calculate() // 6
```

## Extension classes
`tlang` also supports extensions classes using the `extension` keyword which allows
extending existing classes with new methods. `this` inside the extension class will refer
to the instance of the object being extended.

```tlang
extension java::lang::String =
	Def WithPrefix(s: String) = s + this

println("123".WithPrefix("ABC")) // ABC123
```

This also works for all types including primitive types like `Int` and `Double` but does not 
currently support generics. 

## Static methods and fields
`tlang` supports static methods and fields:

```tlang
class A =
	Val static x = 5
	Def static F() = 5 + x

println(A::x) // 5
println(A::F()) // 10
```
