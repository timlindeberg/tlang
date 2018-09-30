# Types
`tlang` is a statically typed language meaning that types are checked at compile time.
This means that every variable has to have a type and every function needs a
return type. However, always declaring types manually can be tedious and can also
reduce readability in many cases where the type is duplicate, for instance when
constructing a new value. Because of this `tlang` supports type inference.

## Primitive types

There are `6` primitive types in `tlang`:

* `Int`
* `Long`
* `Char`
* `Float`
* `Double`
* `Bool`

However, `tlang` doesn't treat primitive types different from other types, even
though they are implemented as such under the hood to give good performance.
Primitive types in `tlang` are subclasses of `Object` just like classes.
To preserve compatibility with `Java`, `tlang` does not define it's own `Object`
type but instead uses `java::lang::Object` as the base of all types. In the same way
`String`:s in `tlang` are instances of `java::lang::String`. There does however 
exist extensions to the these classes so `Object`:s and `String`:s have more methods
`tlang` than they do in `Java`. 

## Type inference
The compiler can figure out the type of most variables and functions. Because of this,
declaring types is often optional. `tlang` uses a syntax similar to `Scala` or `Kotlin`
where types are declared after the variable declaration which makes it convenient
to skip type declarations.

```tlang
val x: Int = 5
val x = 5 // Type is infered to be Int since the literal 5 is of type Int

class A =

	val x: Int = 5 
	val y = 5 // Type is infered to be Int

	Def MyFunction(): String = "5"
	Def MyFunction2() = "5" // Return type is infered to be String

	// Compilation error since the return type doesn't
	// match the declared type
	Def MyFunction3(): Int = "5" 
```

Type inference also works for more advanced cases, like recursion and multiple different
return types. If multiple different types are returned from a function, the closest
common parent will be inferred (which can be type `Object` if no closer parent can be found).


```tlang

class A
class B: A
class C: A

class Test =

	// Infers type String
	Def MyRecursiveFunction(x: Int) = 
		if (x < 0)
			return "Finished!"
		return MyRecursiveFunction(x - 1)

	// Infers type Object
	Def MyFunction(x: Int) = x < 5 ? "ABC" : 123 

	// Infers type A since both B and C are of type A
	Def MyFunction2(x: Int) = x < 5 ? new B() : new C()

	// Infers type B?
	Def MyFunction3(x: Int) = x < 5 ? new B() : null
```
