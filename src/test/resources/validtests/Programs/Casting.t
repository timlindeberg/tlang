var a: A
a = new B()
(a as B).testB()

class A =

	Def test(): Unit = println("A")

	Def testA(): Unit = println("A")

class B : A =

	Def test(): Unit = println("B")

	Def testB(): Unit = println("B") // res: B

class C : B =

	Def test(): Unit = println("C")

	Def testC(): Unit = println("C")