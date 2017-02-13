// Res: C
// Res: Base2
new C().Print()

trait Base1 {
    Def Print() = println("Base1")
}

trait A : Base1 {
    Def Print() = {
        println("A")
        super.Print()
    }
}

trait B : Base1 {
    Def Print() = {
        println("B")
        super.Print()
    }
}

class Base2 {
    Def Print() = println("Base2")
}

class C : Base2, A, B {
    Def Print() = {
        println("C")
        super.Print()
    }
}
