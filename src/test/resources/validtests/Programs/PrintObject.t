println(new Foo().bar(new A())) // res: Print A!

class Foo =
	Def bar(a: Object): Object =
		return a

class A =
	Def toString(): String =
		return "Print A!"
