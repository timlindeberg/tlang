package T::lang

class Char {

    Def new()                = {}
    Def new(v: Char)         = {}
    Def implicit new(v: Int) = {}

    // Arithmetic operators
    Def +(a: Char,   b: Char)  : Int    = {}
    Def +(a: Char,   b: Double): Double = {}
    Def +(a: Double, b: Char)  : Double = {}
    Def +(a: Char,   b: Float) : Float  = {}
    Def +(a: Float,  b: Char)  : Float  = {}
    Def +(a: Char,   b: Long)  : Long   = {}
    Def +(a: Long,   b: Char)  : Long   = {}
    Def +(a: Char,   b: Int)   : Int    = {}
    Def +(a: Int,    b: Char)  : Int    = {}
    
    Def -(a: Char,   b: Char)  : Int    = {}
    Def -(a: Char,   b: Double): Double = {}
    Def -(a: Double, b: Char)  : Double = {}
    Def -(a: Char,   b: Float) : Float  = {}
    Def -(a: Float,  b: Char)  : Float  = {}
    Def -(a: Char,   b: Long)  : Long   = {}
    Def -(a: Long,   b: Char)  : Long   = {}
    Def -(a: Char,   b: Int)   : Int    = {}
    Def -(a: Int,    b: Char)  : Int    = {}
    
    Def *(a: Char,   b: Char)  : Int    = {}
    Def *(a: Char,   b: Double): Double = {}
    Def *(a: Double, b: Char)  : Double = {}
    Def *(a: Char,   b: Float) : Float  = {}
    Def *(a: Float,  b: Char)  : Float  = {}
    Def *(a: Char,   b: Long)  : Long   = {}
    Def *(a: Long,   b: Char)  : Long   = {}
    Def *(a: Char,   b: Int)   : Int    = {}
    Def *(a: Int,    b: Char)  : Int    = {}
    
    Def /(a: Char,   b: Char)  : Int    = {}
    Def /(a: Char,   b: Double): Double = {}
    Def /(a: Double, b: Char)  : Double = {}
    Def /(a: Char,   b: Float) : Float  = {}
    Def /(a: Float,  b: Char)  : Float  = {}
    Def /(a: Char,   b: Long)  : Long   = {}
    Def /(a: Long,   b: Char)  : Long   = {}
    Def /(a: Char,   b: Int)   : Int    = {}
    Def /(a: Int,    b: Char)  : Int    = {}
    
    Def %(a: Char,   b: Char)  : Int    = {}
    Def %(a: Char,   b: Double): Double = {}
    Def %(a: Double, b: Char)  : Double = {}
    Def %(a: Char,   b: Float) : Float  = {}
    Def %(a: Float,  b: Char)  : Float  = {}
    Def %(a: Char,   b: Long)  : Long   = {}
    Def %(a: Long,   b: Char)  : Long   = {}
    Def %(a: Char,   b: Int)   : Int    = {}
    Def %(a: Int,    b: Char)  : Int    = {}
    
    // Logical operators
    
    Def &(a: Char, b: Char): Int =  {}
    Def &(a: Char, b: Long): Long = {}
    Def &(a: Long, b: Char): Long = {}
    Def &(a: Char, b: Int) : Int =  {}
    Def &(a: Int,  b: Char): Int =  {}

    Def |(a: Char, b: Char): Int =  {}
    Def |(a: Char, b: Long): Long = {}
    Def |(a: Long, b: Char): Long = {}
    Def |(a: Char, b: Int) : Int =  {}
    Def |(a: Int,  b: Char): Int =  {}
    
    Def ^(a: Char, b: Char): Int =  {}
    Def ^(a: Char, b: Long): Long = {}
    Def ^(a: Long, b: Char): Long = {}
    Def ^(a: Char, b: Int) : Int =  {}
    Def ^(a: Int,  b: Char): Int =  {}
    
    // Shift operators

    Def <<(a: Char, b: Char): Int =  {}
    Def <<(a: Char, b: Long): Long = {}
    Def <<(a: Long, b: Char): Long = {}
    Def <<(a: Char, b: Int) : Int =  {}
    Def <<(a: Int,  b: Char): Int =  {}

    Def >>(a: Char, b: Char): Int =  {}
    Def >>(a: Char, b: Long): Long = {}
    Def >>(a: Long, b: Char): Long = {}
    Def >>(a: Char, b: Int) : Int =  {}
    Def >>(a: Int,  b: Char): Int =  {}

    // Comparison operators

    Def <(a: Char,   b: Char)  : Bool = {}
    Def <(a: Char,   b: Double): Bool = {}
    Def <(a: Double, b: Char)  : Bool = {}
    Def <(a: Char,   b: Float) : Bool = {}
    Def <(a: Float,  b: Char)  : Bool = {}
    Def <(a: Char,   b: Long)  : Bool = {}
    Def <(a: Long,   b: Char)  : Bool = {}
    Def <(a: Char,   b: Int)   : Bool = {}
    Def <(a: Int,    b: Char)  : Bool = {}
    
    Def <=(a: Char,   b: Char)  : Bool = {}
    Def <=(a: Char,   b: Double): Bool = {}
    Def <=(a: Double, b: Char)  : Bool = {}
    Def <=(a: Char,   b: Float) : Bool = {}
    Def <=(a: Float,  b: Char)  : Bool = {}
    Def <=(a: Char,   b: Long)  : Bool = {}
    Def <=(a: Long,   b: Char)  : Bool = {}
    Def <=(a: Char,   b: Int)   : Bool = {}
    Def <=(a: Int,    b: Char)  : Bool = {}

    Def >(a: Char,   b: Char)  : Bool = {}
    Def >(a: Char,   b: Double): Bool = {}
    Def >(a: Double, b: Char)  : Bool = {}
    Def >(a: Char,   b: Float) : Bool = {}
    Def >(a: Float,  b: Char)  : Bool = {}
    Def >(a: Char,   b: Long)  : Bool = {}
    Def >(a: Long,   b: Char)  : Bool = {}
    Def >(a: Char,   b: Int)   : Bool = {}
    Def >(a: Int,    b: Char)  : Bool = {}
    
    Def >=(a: Char,   b: Char)  : Bool = {}
    Def >=(a: Char,   b: Double): Bool = {}
    Def >=(a: Double, b: Char)  : Bool = {}
    Def >=(a: Char,   b: Float) : Bool = {}
    Def >=(a: Float,  b: Char)  : Bool = {}
    Def >=(a: Char,   b: Long)  : Bool = {}
    Def >=(a: Long,   b: Char)  : Bool = {}
    Def >=(a: Char,   b: Int)   : Bool = {}
    Def >=(a: Int,    b: Char)  : Bool = {}
    
    // Equals operators
    
    Def ==(a: Char,   b: Char)  : Bool = {}
    Def ==(a: Char,   b: Double): Bool = {}
    Def ==(a: Double, b: Char)  : Bool = {}
    Def ==(a: Char,   b: Float) : Bool = {}
    Def ==(a: Float,  b: Char)  : Bool = {}
    Def ==(a: Char,   b: Long)  : Bool = {}
    Def ==(a: Long,   b: Char)  : Bool = {}
    Def ==(a: Char,   b: Int)   : Bool = {}
    Def ==(a: Int,    b: Char)  : Bool = {}
       
    Def !=(a: Char,   b: Char)  : Bool = {}
    Def !=(a: Char,   b: Double): Bool = {}
    Def !=(a: Double, b: Char)  : Bool = {}
    Def !=(a: Char,   b: Float) : Bool = {}
    Def !=(a: Float,  b: Char)  : Bool = {}
    Def !=(a: Char,   b: Long)  : Bool = {}
    Def !=(a: Long,   b: Char)  : Bool = {}
    Def !=(a: Char,   b: Int)   : Bool = {}
    Def !=(a: Int,    b: Char)  : Bool = {}

    // Unary operators
    Def #(a: Char) : Int =  {}
    Def -(a: Char) : Int =  {}
    Def ~(a: Char) : Int =  {}
    Def ++(a: Char): Char = {}
    Def --(a: Char): Char = {}

}