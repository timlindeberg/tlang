class A: B, C {

    Def Test1() = super.Test() // res: T2029
    Def Test2() = super.A // res: T2030

    Def Test3() = super<B>.Test() // res: T2001
    Def Test4() = super<C>.A // res: T2003


}

trait B {}

trait C {}