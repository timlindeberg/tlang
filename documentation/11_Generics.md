# Generics
Even though `tlang` is a JVM language, it's generics are more similar to the 
templates of `C++` than `Java`'s type erasure. For each instantiation of a template
a custom class is generated. This means that there is no performance overhead or boxing,
if you instantiate a generic class with the `Int` type it will use the primitive data type.
Another advantage of this method is that type information is available during runtime,
meaning it's possible for instance to overload on different instantiations of a generic
class or to use the `new` operator to instantiate the generic type.

A generic class is declared using the following syntax:

```tlang
class GenericClass<T> =
	var t: T

	Def new() =
		t = new T(); // Would not be possible using type erasure

	Def new(t: T) =
		this.t = t

	Def ToString() = t.ToString()


Def MyFunction1(x: GenericClass<Int>)    = println("Int class: " + x)
Def MyFunction1(x: GenericClass<Double>) = println("Double class: " + x)

MyFunction(new GenericClass<Int>())        // Prints "Int class: 0"
MyFunction(new GenericClass<Double>(10.0)) // Prints "Double class: 10.0"
```

An obvious disadvantage to this method is that it generates many classes which
can increase the startup time of an application. It can therefore be good to
be careful with the number of generic types in generic classes.

Generics can currently only be applied to classes and traits but will be available
for extension classes and for methods in the future.
