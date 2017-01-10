val a: A? = GetA()

if(a) {

    if(a) { // res: F1001
        println()
    }

    if(!a) {  // res: F1001
        println()
    }

    if(a != null) { // res: F1001
        println()
    }

    if(a == null) { // res: F1001
        println()
    }
}


if(!a) {
    if(a) { // res: F1001
        println()
    }

    if(!a) {  // res: F1001
        println()
    }

    if(a != null) { // res: F1001
        println()
    }

    if(a == null) { // res: F1001
        println()
    }
}

Def GetA() = true ? new A() : null

class A {

    Def Test() = println("Test")

}