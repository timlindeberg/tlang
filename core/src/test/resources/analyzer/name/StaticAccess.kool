class B {

    Def A() = 1
}

class A : B {
    var x: Int = 1

    Def static Test() = {
        var t = this // res: N2013
        var a = super.A() // res: N2020
        return x + 1 // res: N2011
    }
}