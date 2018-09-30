# Scoping
`tlang` uses lexical scoping meaning that a symbol will resolve to the 
closest declaration. Local variables have the highest precedence, then 
method arguments and then fields.

```tlang
class A =

	val a = 5

	Def MyFunction() =
		println(a) // prints(5)

	Def MyFunction(a: Int) =
		println(a) // a refers to the method argument
		val a = 10
		println(a) // prints(10)
		for(var i = 0; i < a; i++) // a refers to the the outer scope
			val a = 15
			println(a) // prints 15, 10 times
```

To refer to static variable one can prefix the variable with class name using 
the scoping operator `::`. The prefix can be skipped when in a static context

```tlang
class A =
	val static a = 5

	Def static X() = a + 5 // Refers to static field 'a' since X() is also static

println(A::a) // refers to the static field 'a' in class A
```
