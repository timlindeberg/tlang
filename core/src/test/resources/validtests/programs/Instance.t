var a: A;
var b: B;
var c: A;
var arr: Int[] = new Int[5];
a = new A();
b = new B();
c = new B();

println(a is A); // res: true
println(a is B); // res: false
println(b is B); // res: true
println(b is A); // res: true
println(c is A); // res: true
println(c is B); // res: true
println(arr is Int[]); // res: true
println(arr is A); // res: false

class A
class B : A
