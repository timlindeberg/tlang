for(val v in new A()) // res: T2032
    println(v)

for(val v in new B()) // res: T2032
    println(v)


for(val v in new C()) // res: T2032
    println(v)


for(val v in new D()) // res: T2032
    println(v)

for(val v in new E())
    println(v)

class A {}

class B {
    Def Iterator(): Int = 5
}

class C {
    Def Iterator(): FalseIterator = new FalseIterator()
}

class D {

    Def Iterator(): FalseIterator2 = new FalseIterator2()

}

class E {
    Def Iterator(): Iterator = new Iterator()
}

class FalseIterator {
}


class FalseIterator2 {

    Def HasNext(): Int = 5
    Def Next(): Int = 1

}

class Iterator {

    Def HasNext(): Bool = true
    Def Next(): Int = 5
}