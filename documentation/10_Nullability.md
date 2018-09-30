# Nullability
In `tlang` not every value can be `null`, only types that are explicitly declared as nullable.
This removes a whole category of bugs since it's always clear whether a value exists or not. A
nullable type is declared by appending a `?` to the type.

```tlang
val x: Int? = Math.random() > 0.5 ? 1 : null

class A =
	Val Y: String?
	Def X(x: Int?) = ;
```

## Accessing nullable variables
Accessing a variable that can be `null` is a compilation error. `tlang` uses data flow analysis to
determine whether a variable can be `null` or not.

```tlang
val a: A? = Math.random() > 0.5 ? new A() : null

a.A() // compilation error, since a might be null

if(a)
	a.A() // this is okay since we know that a is not null
```

A nullable type will evaluate to `false` in a condition if it's `null` and `true` otherwise.
Using a direct check is not the only way that the `tlang` compiler can know whether a variable is
`null` or not.

```tlang
val a: A? = Math.random() > 0.5 ? new A() : null

if(!a)
	return

a.X() // Safe

for(var i = 0; i < 5; i++)
	if(i < 3)
		continue
	a.X() // Safe

if(false || a != null)
	a.X() // Safe

val b: A? = Math.random() > 0.5 ? new A() : null

if(!(a == null || b == null ))
	a.X() // Safe
	b.X() // Safe
```

## Null extraction
`tlang` should be able to determine whether a variable is `null` but for the cases when it can't
know the Extract Nullable operator, `!!` can be used to tell the `tlang` compiler that a value is not 
`null`. If the expression was `null` anyway a `NullPointerException` will be thrown.

```tlang
var x: Int? = Math.random() > 0.5 ? new A() : null
val y: Int = x!! // Feeling pretty sure that we didn't get null 
```

## Safe access and the Elvis operator
Since checking nullable variables all the time can be tedious, `tlang` supports a safe access
operator, `?.`. With this operator the value will either be the field or the result of the 
accessed method or `null` if the accessed expression was `null`. The safe access operator can 
only be used on nullable types.

```tlang
val x: D? = a?.b?.GetC()?.d // x is null if any of a, b or GetC() is null
```

Together with the elvis operator, this provides a very concise and safe way of accessing potentially
`null` expressions.

```tlang
val abc = new D() 
val x: D = a?.b?.GetC()?.d ?: abc // If any of a, b or GetC() is null, x is assigned abc 
```
