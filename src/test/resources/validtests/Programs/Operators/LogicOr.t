var b: Bool = true;
var c: Char = 'a';
var i: Int = 2;
var l: Long = 2l;

// Bool
println(b | b);   // res: true
println(b | false);   // res: true
println(false | false);   // res: false

// Char
println(c | c);   // res: 97

// Int
println(i | i);   // res: 2
println(i | l);   // res: 2
println(i | c);   // res: 99

// Long
println(l | l);   // res: 2
println(l | c);   // res: 99

class A =
	Def toString(): String = return "A";

