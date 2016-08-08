var a: A? = null

var i = 0
if(i == 5){
    a = GetA()
}

a.Test() // res: F2000

if(a){
    a.Test()
    if(i == 5){
        a = GetA()
    } else {
        println()
    }
    a.Test() // res: F2000
}

if(a){

    while(true){
        a = GetA()
    }
    a.Test() // res: F2000
} else {
    while(true){
        a = GetA()
    }
    a.Test() // res: F2000
}

if(a) {
    if(1 == 5){
        var j = 0
        if(i == 0){
            j = 5
            a = null
            println()
        }else{
            j = 5
        }
        a.Test() // res: F2000
    }else{
        println()
    }
    a.Test() // res: F2000
}

if(a) {

    if(1 == 5){
        a = new A()
    }else{
        println()
    }
    a.Test()
}

if(!a){
    if(1 == 5){
        a = null
    }else{
        println()
    }
    a.Test() // res: F2001
}

a = new A()

if(a) {
    a = null
    return
}

a.Test()

a = GetA()

if(a){
    a = null
}else{
    return
}

a.Test() // res: F2001

a = GetA()

if(a){
    return
}else{
    a = null
}
a.Test() // res: F2001

Def GetA(): A? = null

class A {
    Def Test() = println()
}