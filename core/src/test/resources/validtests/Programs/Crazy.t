println(new A().bar());     // res: false
println(new A().bar2());    // res: true
println(new A().T());       // res: 0
println(new B().foo());     // res: 42
println(new B().a());       // res: false
println(new B().b());       // res: false
println(new Test().init()); // res: 1

class A =
	var protected value: Int
	var protected a: Bool
	var protected b: Bool

	Def bar(): Bool = !a && b;
	Def bar2(): Bool = !(a && b);
	Def T() : Int = 0;

	Def foo(): Int =
		var value: Bool;
		value = false;
		41;

class B : A =
	Def foo(): Int =
		value = 42
		return value;

	Def a(): Bool =
		return a;

	Def b(): Bool =
		return b;

class Test =
	Def init() : Int =
		var expr1 : Bool;
		var expr2 : Bool;
		var a : Int = 5;
		expr1 = true;
		expr2 = true;
		if(expr1)
			if(expr2)
				a=1;
			else
				a=2;

		if(expr1) if(expr2) a=1; else a=2;
		if(expr1)
			if(expr2)
				a=1;
		else
			a=2;
		return a;
