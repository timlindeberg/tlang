package T::lang

class Float =

	Def new()                 = ;
	Def new(v: Float)         = ;
	Def implicit new(v: Int)  = ;
	Def implicit new(v: Long) = ;
	Def implicit new(v: Char) = ;

	// Arithmetic operators

	Def +(a: Float,  b: Float) : Float  = ;
	Def +(a: Float,  b: Double): Double = ;
	Def +(a: Double, b: Float) : Double = ;
	Def +(a: Float,  b: Int)   : Float  = ;
	Def +(a: Int,    b: Float) : Float  = ;
	Def +(a: Float,  b: Long)  : Float  = ;
	Def +(a: Long,   b: Float) : Float  = ;
	Def +(a: Float,  b: Char)  : Float  = ;
	Def +(a: Char,   b: Float) : Float  = ;
	
	Def -(a: Float,  b: Float) : Float  = ;
	Def -(a: Float,  b: Double): Double = ;
	Def -(a: Double, b: Float) : Double = ;
	Def -(a: Float,  b: Int)   : Float  = ;
	Def -(a: Int,    b: Float) : Float  = ;
	Def -(a: Float,  b: Long)  : Float  = ;
	Def -(a: Long,   b: Float) : Float  = ;
	Def -(a: Float,  b: Char)  : Float  = ;
	Def -(a: Char,   b: Float) : Float  = ;
	
	Def *(a: Float,  b: Float) : Float  = ;
	Def *(a: Float,  b: Double): Double = ;
	Def *(a: Double, b: Float) : Double = ;
	Def *(a: Float,  b: Int)   : Float  = ;
	Def *(a: Int,    b: Float) : Float  = ;
	Def *(a: Float,  b: Long)  : Float  = ;
	Def *(a: Long,   b: Float) : Float  = ;
	Def *(a: Float,  b: Char)  : Float  = ;
	Def *(a: Char,   b: Float) : Float  = ;
	
	Def /(a: Float,  b: Float) : Float  = ;
	Def /(a: Float,  b: Double): Double = ;
	Def /(a: Double, b: Float) : Double = ;
	Def /(a: Float,  b: Int)   : Float  = ;
	Def /(a: Int,    b: Float) : Float  = ;
	Def /(a: Float,  b: Long)  : Float  = ;
	Def /(a: Long,   b: Float) : Float  = ;
	Def /(a: Float,  b: Char)  : Float  = ;
	Def /(a: Char,   b: Float) : Float  = ;
	
	Def %(a: Float,  b: Float) : Float  = ;
	Def %(a: Float,  b: Double): Double = ;
	Def %(a: Double, b: Float) : Double = ;
	Def %(a: Float,  b: Int)   : Float  = ;
	Def %(a: Int,    b: Float) : Float  = ;
	Def %(a: Float,  b: Long)  : Float  = ;
	Def %(a: Long,   b: Float) : Float  = ;
	Def %(a: Float,  b: Char)  : Float  = ;
	Def %(a: Char,   b: Float) : Float  = ;
	
	// Comparison operators

	Def <(a: Float,  b: Float) : Bool = ;
	Def <(a: Float,  b: Double): Bool = ;
	Def <(a: Double, b: Float) : Bool = ;
	Def <(a: Float,  b: Int)   : Bool = ;
	Def <(a: Int,    b: Float) : Bool = ;
	Def <(a: Float,  b: Long)  : Bool = ;
	Def <(a: Long,   b: Float) : Bool = ;
	Def <(a: Float,  b: Char)  : Bool = ;
	Def <(a: Char,   b: Float) : Bool = ;
	
	Def <=(a: Float,  b: Float) : Bool = ;
	Def <=(a: Float,  b: Double): Bool = ;
	Def <=(a: Double, b: Float) : Bool = ;
	Def <=(a: Float,  b: Int)   : Bool = ;
	Def <=(a: Int,    b: Float) : Bool = ;
	Def <=(a: Float,  b: Long)  : Bool = ;
	Def <=(a: Long,   b: Float) : Bool = ;
	Def <=(a: Float,  b: Char)  : Bool = ;
	Def <=(a: Char,   b: Float) : Bool = ;

	Def >(a: Float,  b: Float) : Bool = ;
	Def >(a: Float,  b: Double): Bool = ;
	Def >(a: Double, b: Float) : Bool = ;
	Def >(a: Float,  b: Int)   : Bool = ;
	Def >(a: Int,    b: Float) : Bool = ;
	Def >(a: Float,  b: Long)  : Bool = ;
	Def >(a: Long,   b: Float) : Bool = ;
	Def >(a: Float,  b: Char)  : Bool = ;
	Def >(a: Char,   b: Float) : Bool = ;
	
	Def >=(a: Float,  b: Float) : Bool = ;
	Def >=(a: Float,  b: Double): Bool = ;
	Def >=(a: Double, b: Float) : Bool = ;
	Def >=(a: Float,  b: Int)   : Bool = ;
	Def >=(a: Int,    b: Float) : Bool = ;
	Def >=(a: Float,  b: Long)  : Bool = ;
	Def >=(a: Long,   b: Float) : Bool = ;
	Def >=(a: Float,  b: Char)  : Bool = ;
	Def >=(a: Char,   b: Float) : Bool = ;
	
	// Equals operators
	
	Def ==(a: Float,  b: Float) : Bool = ;
	Def ==(a: Float,  b: Double): Bool = ;
	Def ==(a: Double, b: Float) : Bool = ;
	Def ==(a: Float,  b: Int)   : Bool = ;
	Def ==(a: Int,    b: Float) : Bool = ;
	Def ==(a: Float,  b: Long)  : Bool = ;
	Def ==(a: Long,   b: Float) : Bool = ;
	Def ==(a: Float,  b: Char)  : Bool = ;
	Def ==(a: Char,   b: Float) : Bool = ;
	   
	Def !=(a: Float,  b: Float) : Bool = ;
	Def !=(a: Float,  b: Double): Bool = ;
	Def !=(a: Double, b: Float) : Bool = ;
	Def !=(a: Float,  b: Int)   : Bool = ;
	Def !=(a: Int,    b: Float) : Bool = ;
	Def !=(a: Float,  b: Long)  : Bool = ;
	Def !=(a: Long,   b: Float) : Bool = ;
	Def !=(a: Float,  b: Char)  : Bool = ;
	Def !=(a: Char,   b: Float) : Bool = ;

	// Unary operators
	Def #(a: Float) : Int   = ;
	Def -(a: Float) : Float = ;
	Def ++(a: Float): Float = ;
	Def --(a: Float): Float = ;
