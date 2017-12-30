trait A

class B: A
class C: A

var a: A
if(true) a = new B()
else     a = new C()

println(a)