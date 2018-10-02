# Control Flow
`tlang` supports most normal imperative control flow mechanisms:

## If
```tlang
if (a < b)
	println("less")
```

## While
```tlang
while (true)
	println("true")
```

## For loops
There are two types of for loops in `tlang`, the regular for loop with an initiation list,
a condition, and a list of post operations:
```tlang
for (var i = 0; i < 5; i++)
	println(i)

for (var i = 0, j = 5, k = 10; i < 5 && j < 15 && k < 20; i++, j++, k++)
	println(i + j + k)
```

There are also foreach loops which can be used on `Iterable` types:

```tlang
val vec: t::lang::Vector<Int> = [ 1, 2, 3, 4, 5 ]
for (val i: Int in vec) 
	println(i)

// The type can also be inferred
for (val i in vec) 
	println(i)
```

A type `Foo` is `Iterable` if and only if the following is true:

* `Foo` implements a method called `Iterator()` accepting zero arguments.
* `Iterator()` returns an `Iterator` object.
* The `Iterator` object implements the following functions:
```tlang
Def HasNext(): Bool 
Def Next(): T 
```
A type which implements the `t::lang::Iterable` interface will always uphold 
these criteria but it is not needed to be `Iterable`.

## Do while
`tlang` does not currently support do while loops.
