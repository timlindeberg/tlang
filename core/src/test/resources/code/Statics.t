new H().run();

class H =

	Def run(): Unit =
		val a: A = new A();
		a.I = 1;
		println(a.I);       // res: 1
		A.I = 2;
		println(A.I);       // res: 2

		println(a.Test());  // res: 2
		a.Test2(3);
		println(A.I);       // res: 3

		println(A.Test3()); // res: Test3
		println(a.Test3()); // res: Test3
		println(a.Test4()); // res: Test3

class A =

	Var static I: Int;

	Def Test(): Int =
		return I;

	Def Test2(i: Int): Unit =
		I = i;

	Def static Test3(): String =
		return "Test3";

	Def Test4(): String =
		return Test3();
