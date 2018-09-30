# Implicit Conversions
Implicit conversions can be defined by declaring a constructor with a single argument
and using the key word `implicit`. This will let the type be constructed implicitly from 
the other type. While this feature can be very handy at times, one should be careful 
with introducing too many implicit conversions since it can sometimes lead to unexpected 
behaviour.

```tlang
class A =

	var s: String

	Def new(x: Int) =
		s = x.ToString()

	Def implicit new(x: String) =
		s = x

val a: A = "5"        // Implicitly constructs an 'A' using the String constructor
val b: A = new A("5") // Same as the line above
val c: A = 5          // Compilation error since there is no implicit conversion declared from Int to A

Def MyFunction(a: A)
	println(a)
 
MyFunction("100") // Implicitly constructs a new A and passes it to the function
```
