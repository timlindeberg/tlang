package T::lang

class BoolRef {

    var v: Bool

    Def static ValueOf(v: Bool) = new BoolRef(v)
    Def Value() = v

    Def toString() = java::lang::String.valueOf(v)
    def new(v: Bool) = (this.v = v)

}