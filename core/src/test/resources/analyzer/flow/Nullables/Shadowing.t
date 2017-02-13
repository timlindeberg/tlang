val a = GetA()
if(a){
    a.Test()
    val a = GetA()
    a.Test() // res: F2000
    if(a){
        a.Test()
    } else {
        a.Test() // res: F2001
        val a = GetA()
        a.Test() // res: F2000
        if(a)
            a.Test()
    }
}

/*------------------------------------------------------------------------*/

Def GetA(): A? = null

class A {

    Def Test() = println("Test")

}
