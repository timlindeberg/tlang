package T::lang

class Int {

    Def new()                 = ;
    Def new(v: Int)           = ;
    Def implicit new(v: Char) = ;

    // Arithmetic operators

    Def +(a: Int,    b: Int)   : Int    = ;
    Def +(a: Int,    b: Double): Double = ;
    Def +(a: Double, b: Int)   : Double = ;
    Def +(a: Int,    b: Float) : Float  = ;
    Def +(a: Float,  b: Int)   : Float  = ;
    Def +(a: Int,    b: Long)  : Long   = ;
    Def +(a: Long,   b: Int)   : Long   = ;
    Def +(a: Int,    b: Char)  : Int    = ;
    Def +(a: Char,   b: Int)   : Int    = ;
    
    Def -(a: Int,    b: Int)   : Int    = ;
    Def -(a: Int,    b: Double): Double = ;
    Def -(a: Double, b: Int)   : Double = ;
    Def -(a: Int,    b: Float) : Float  = ;
    Def -(a: Float,  b: Int)   : Float  = ;
    Def -(a: Int,    b: Long)  : Long   = ;
    Def -(a: Long,   b: Int)   : Long   = ;
    Def -(a: Int,    b: Char)  : Int    = ;
    Def -(a: Char,   b: Int)   : Int    = ;
    
    Def *(a: Int,    b: Int)   : Int    = ;
    Def *(a: Int,    b: Double): Double = ;
    Def *(a: Double, b: Int)   : Double = ;
    Def *(a: Int,    b: Float) : Float  = ;
    Def *(a: Float,  b: Int)   : Float  = ;
    Def *(a: Int,    b: Long)  : Long   = ;
    Def *(a: Long,   b: Int)   : Long   = ;
    Def *(a: Int,    b: Char)  : Int    = ;
    Def *(a: Char,   b: Int)   : Int    = ;
    
    Def /(a: Int,    b: Int)   : Int    = ;
    Def /(a: Int,    b: Double): Double = ;
    Def /(a: Double, b: Int)   : Double = ;
    Def /(a: Int,    b: Float) : Float  = ;
    Def /(a: Float,  b: Int)   : Float  = ;
    Def /(a: Int,    b: Long)  : Long   = ;
    Def /(a: Long,   b: Int)   : Long   = ;
    Def /(a: Int,    b: Char)  : Int    = ;
    Def /(a: Char,   b: Int)   : Int    = ;
    
    Def %(a: Int,    b: Int)   : Int    = ;
    Def %(a: Int,    b: Double): Double = ;
    Def %(a: Double, b: Int)   : Double = ;
    Def %(a: Int,    b: Float) : Float  = ;
    Def %(a: Float,  b: Int)   : Float  = ;
    Def %(a: Int,    b: Long)  : Long   = ;
    Def %(a: Long,   b: Int)   : Long   = ;
    Def %(a: Int,    b: Char)  : Int    = ;
    Def %(a: Char,   b: Int)   : Int    = ;
    
    // Logical operators
    
    Def &(a: Int,  b: Int) : Int  = ;
    Def &(a: Int,  b: Long): Long = ;
    Def &(a: Long, b: Int) : Long = ;
    Def &(a: Int,  b: Char): Int  = ;
    Def &(a: Char, b: Int) : Int  = ;

    Def |(a: Int,  b: Int) : Int  = ;
    Def |(a: Int,  b: Long): Long = ;
    Def |(a: Long, b: Int) : Long = ;
    Def |(a: Int,  b: Char): Int  = ;
    Def |(a: Char, b: Int) : Int  = ;

    Def ^(a: Int,  b: Int) : Int  = ;
    Def ^(a: Int,  b: Long): Long = ;
    Def ^(a: Long, b: Int) : Long = ;
    Def ^(a: Int,  b: Char): Int  = ;
    Def ^(a: Char, b: Int) : Int  = ;
    
    // Shift operators

    Def <<(a: Int,  b: Int) : Int  = ;
    Def <<(a: Int,  b: Long): Long = ;
    Def <<(a: Long, b: Int) : Long = ;
    Def <<(a: Int,  b: Char): Int  = ;
    Def <<(a: Char, b: Int) : Int  = ;

    Def >>(a: Int,  b: Int) : Int  = ;
    Def >>(a: Int,  b: Long): Long = ;
    Def >>(a: Long, b: Int) : Long = ;
    Def >>(a: Int,  b: Char): Int  = ;
    Def >>(a: Char, b: Int) : Int  = ;

    // Comparison operators

    Def <(a: Int,    b: Int)   : Bool = ;
    Def <(a: Int,    b: Double): Bool = ;
    Def <(a: Double, b: Int)   : Bool = ;
    Def <(a: Int,    b: Float) : Bool = ;
    Def <(a: Float,  b: Int)   : Bool = ;
    Def <(a: Int,    b: Long)  : Bool = ;
    Def <(a: Long,   b: Int)   : Bool = ;
    Def <(a: Int,    b: Char)  : Bool = ;
    Def <(a: Char,   b: Int)   : Bool = ;
    
    Def <=(a: Int,    b: Int)   : Bool = ;
    Def <=(a: Int,    b: Double): Bool = ;
    Def <=(a: Double, b: Int)   : Bool = ;
    Def <=(a: Int,    b: Float) : Bool = ;
    Def <=(a: Float,  b: Int)   : Bool = ;
    Def <=(a: Int,    b: Long)  : Bool = ;
    Def <=(a: Long,   b: Int)   : Bool = ;
    Def <=(a: Int,    b: Char)  : Bool = ;
    Def <=(a: Char,   b: Int)   : Bool = ;

    Def >(a: Int,    b: Int)   : Bool = ;
    Def >(a: Int,    b: Double): Bool = ;
    Def >(a: Double, b: Int)   : Bool = ;
    Def >(a: Int,    b: Float) : Bool = ;
    Def >(a: Float,  b: Int)   : Bool = ;
    Def >(a: Int,    b: Long)  : Bool = ;
    Def >(a: Long,   b: Int)   : Bool = ;
    Def >(a: Int,    b: Char)  : Bool = ;
    Def >(a: Char,   b: Int)   : Bool = ;
    
    Def >=(a: Int,    b: Int)   : Bool = ;
    Def >=(a: Int,    b: Double): Bool = ;
    Def >=(a: Double, b: Int)   : Bool = ;
    Def >=(a: Int,    b: Float) : Bool = ;
    Def >=(a: Float,  b: Int)   : Bool = ;
    Def >=(a: Int,    b: Long)  : Bool = ;
    Def >=(a: Long,   b: Int)   : Bool = ;
    Def >=(a: Int,    b: Char)  : Bool = ;
    Def >=(a: Char,   b: Int)   : Bool = ;
    
    // Equals operators
    
    Def ==(a: Int,    b: Int)   : Bool = ;
    Def ==(a: Int,    b: Double): Bool = ;
    Def ==(a: Double, b: Int)   : Bool = ;
    Def ==(a: Int,    b: Float) : Bool = ;
    Def ==(a: Float,  b: Int)   : Bool = ;
    Def ==(a: Int,    b: Long)  : Bool = ;
    Def ==(a: Long,   b: Int)   : Bool = ;
    Def ==(a: Int,    b: Char)  : Bool = ;
    Def ==(a: Char,   b: Int)   : Bool = ;
       
    Def !=(a: Int,    b: Int)   : Bool = ;
    Def !=(a: Int,    b: Double): Bool = ;
    Def !=(a: Double, b: Int)   : Bool = ;
    Def !=(a: Int,    b: Float) : Bool = ;
    Def !=(a: Float,  b: Int)   : Bool = ;
    Def !=(a: Int,    b: Long)  : Bool = ;
    Def !=(a: Long,   b: Int)   : Bool = ;
    Def !=(a: Int,    b: Char)  : Bool = ;
    Def !=(a: Char,   b: Int)   : Bool = ;

    // Unary operators
    Def #(a: Int) : Int = ;
    Def -(a: Int) : Int = ;
    Def ~(a: Int) : Int = ;
    Def ++(a: Int): Int = ;
    Def --(a: Int): Int = ;

}