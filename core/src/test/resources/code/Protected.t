var b: B;
var a: A;
b = new B();
a = new A();

println(b.Test(0)); // res: 1
println(a.Test(0)); // res: 1

class A =

	def protected test(i: Int): Int =
		return i + 1;

	Def Test(i: Int): Int =
		return test(i);

class B : A =

	Def Test(i: Int): Int =
		return test(i);
