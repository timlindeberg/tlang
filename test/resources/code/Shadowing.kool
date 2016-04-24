println(new Test1().test(1337)); // res: 20
println(new Test2().test(1337)); // res: 1347

class Test1 {
    var t: Test1;
    var b: String;

    Def test(num : Int): Int = {
        var b: Int;
        var t: Int;
        t = 10;
        b = 10;
        return t + b;
    }
}

class Test2 {
    var t: Test1;
    var b: Int;

    Def test(t : Int): Int = {
        b = 10;
        return t + b;
    }
}
