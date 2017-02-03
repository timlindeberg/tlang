val a: A? = new A()

val i: Int? = a?.i

val a2: A = new A()
i = a2?.i // res: T2034

val b: B? = new B()
i = b?.a?.i

i = b!!.a2?.i // res: T2034

class A {
    Val i = 5
}

class B {

    Val a: A? = new A()
    Val a2: A = new A()

}