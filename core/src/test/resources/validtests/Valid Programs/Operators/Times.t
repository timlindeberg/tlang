var c: Char = 'a';
var i: Int = 2;
var l: Long = 2l;
var f: Float = 2f;
var d: Double = 2.0;

// Char
println(c * c);  // res: 9409

// Int
println(i * i);  // res: 4
println(i * l);  // res: 4
println(i * f);  // res: 4.0
println(i * d);  // res: 4.0
println(i * c);  // res: 194

// Long
println(l * l);  // res: 4
println(l * f);  // res: 4.0
println(l * d);  // res: 4.0
println(l * c);  // res: 194

// Float
println(f * f);  // res: 4.0
println(f * d);  // res: 4.0
println(f * c);  // res: 194.0

// Double
println(d * d);  // res: 4.0
println(d * c);  // res: 194.0

class A =
	Def toString(): String = return "A";
