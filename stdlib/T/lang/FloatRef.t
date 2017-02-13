package T::lang

class FloatRef {

    var v: Float

    Def static ValueOf(v: Float) = new FloatRef(v)
    Def Value() = v

    Def toString() = java::lang::String.valueOf(v)
    def new(v: Float) = (this.v = v)

}