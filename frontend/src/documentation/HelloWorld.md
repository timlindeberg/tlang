# Hello World!
In `tlang` the most simple hello world program is written like:

```tlang
println("Hello world!")
```

A `.t`-file can contain statements, functions and classes in any order. When executing the file statements
are executed in the order that they appear. Methods and classes can be used before they are declared.

Example:

In a file called `HelloWorld.t`:
```tlang
val a = ConstructA()

Def ConstructA() = new A() 

class A = 
	Def Run() = println("Hello World!") 

a.Run() // Hello World!
```

This program can be executed by running `java HelloWorld`.

## Arguments

Arguments can be accessed throug    h the `args` variable:

```tlang
println("Hello " + args[0] + "!") 
```

Executing with `java HelloWorld World` prints `Hello World!`.


## How it works

The `tlang` compiler generates a class called `HelloWorld` (taken from the filename) and puts all the loose statements in
the file in to the main method and all loose functions are included in the class as static functions.
The result looks something like this:

```tlang
class HelloWorld = 

	Def main(args: String[]) =
		val a = ConstructA()
		a.Run() // Hello World!

	Def static ConstructA() = new A() 

class A = 
	Def Run() = println("Hello World!")
```
