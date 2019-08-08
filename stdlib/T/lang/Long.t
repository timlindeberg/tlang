package T::lang

import T::lang::Char
import T::lang::Int
import T::lang::Float
import T::lang::Double
import T::lang::Bool

class Long =

	Def new()                 = ;
	Def new(v: Long)          = ;
	Def implicit new(v: Int)  = ;
	Def implicit new(v: Char) = ;

	// Arithmetic operators

	Def +(a: Long,   b: Long)  : Long   = ;
	Def +(a: Long,   b: Double): Double = ;
	Def +(a: Double, b: Long)  : Double = ;
	Def +(a: Long,   b: Float) : Float  = ;
	Def +(a: Float,  b: Long)  : Float  = ;
	Def +(a: Long,   b: Int)   : Long   = ;
	Def +(a: Int,    b: Long)  : Long   = ;
	Def +(a: Long,   b: Char)  : Long   = ;
	Def +(a: Char,   b: Long)  : Long   = ;

	Def -(a: Long,   b: Long)  : Long   = ;
	Def -(a: Long,   b: Double): Double = ;
	Def -(a: Double, b: Long)  : Double = ;
	Def -(a: Long,   b: Float) : Float  = ;
	Def -(a: Float,  b: Long)  : Float  = ;
	Def -(a: Long,   b: Int)   : Long   = ;
	Def -(a: Int,    b: Long)  : Long   = ;
	Def -(a: Long,   b: Char)  : Long   = ;
	Def -(a: Char,   b: Long)  : Long   = ;

	Def *(a: Long,   b: Long)  : Long   = ;
	Def *(a: Long,   b: Double): Double = ;
	Def *(a: Double, b: Long)  : Double = ;
	Def *(a: Long,   b: Float) : Float  = ;
	Def *(a: Float,  b: Long)  : Float  = ;
	Def *(a: Long,   b: Int)   : Long   = ;
	Def *(a: Int,    b: Long)  : Long   = ;
	Def *(a: Long,   b: Char)  : Long   = ;
	Def *(a: Char,   b: Long)  : Long   = ;

	Def /(a: Long,   b: Long)  : Long   = ;
	Def /(a: Long,   b: Double): Double = ;
	Def /(a: Double, b: Long)  : Double = ;
	Def /(a: Long,   b: Float) : Float  = ;
	Def /(a: Float,  b: Long)  : Float  = ;
	Def /(a: Long,   b: Int)   : Long   = ;
	Def /(a: Int,    b: Long)  : Long   = ;
	Def /(a: Long,   b: Char)  : Long   = ;
	Def /(a: Char,   b: Long)  : Long   = ;

	Def %(a: Long,   b: Long)  : Long   = ;
	Def %(a: Long,   b: Double): Double = ;
	Def %(a: Double, b: Long)  : Double = ;
	Def %(a: Long,   b: Float) : Float  = ;
	Def %(a: Float,  b: Long)  : Float  = ;
	Def %(a: Long,   b: Int)   : Long   = ;
	Def %(a: Int,    b: Long)  : Long   = ;
	Def %(a: Long,   b: Char)  : Long   = ;
	Def %(a: Char,   b: Long)  : Long   = ;


	// Logical operators

	Def &(a: Long,  b: Long): Long = ;
	Def &(a: Long,  b: Int) : Long = ;
	Def &(a: Int,   b: Long): Long = ;
	Def &(a: Long,  b: Char): Long = ;
	Def &(a: Char,  b: Long): Long = ;

	Def |(a: Long,  b: Long): Long = ;
	Def |(a: Long,  b: Int) : Long = ;
	Def |(a: Int,   b: Long): Long = ;
	Def |(a: Long,  b: Char): Long = ;
	Def |(a: Char,  b: Long): Long = ;

	Def ^(a: Long,  b: Long): Long = ;
	Def ^(a: Long,  b: Int) : Long = ;
	Def ^(a: Int,   b: Long): Long = ;
	Def ^(a: Long,  b: Char): Long = ;
	Def ^(a: Char,  b: Long): Long = ;

	// Shift operators

	Def <<(a: Long,  b: Long): Long = ;
	Def <<(a: Long,  b: Int) : Long = ;
	Def <<(a: Int,   b: Long): Long = ;
	Def <<(a: Long,  b: Char): Long = ;
	Def <<(a: Char,  b: Long): Long = ;

	Def >>(a: Long,  b: Long): Long = ;
	Def >>(a: Long,  b: Int) : Long = ;
	Def >>(a: Int,   b: Long): Long = ;
	Def >>(a: Long,  b: Char): Long = ;
	Def >>(a: Char,  b: Long): Long = ;

	// Comparison operators

	Def <(a: Long,   b: Long)  : Bool = ;
	Def <(a: Long,   b: Double): Bool = ;
	Def <(a: Double, b: Long)  : Bool = ;
	Def <(a: Long,   b: Float) : Bool = ;
	Def <(a: Float,  b: Long)  : Bool = ;
	Def <(a: Long,   b: Int)   : Bool = ;
	Def <(a: Int,    b: Long)  : Bool = ;
	Def <(a: Long,   b: Char)  : Bool = ;
	Def <(a: Char,   b: Long)  : Bool = ;

	Def <=(a: Long,   b: Long)  : Bool = ;
	Def <=(a: Long,   b: Double): Bool = ;
	Def <=(a: Double, b: Long)  : Bool = ;
	Def <=(a: Long,   b: Float) : Bool = ;
	Def <=(a: Float,  b: Long)  : Bool = ;
	Def <=(a: Long,   b: Int)   : Bool = ;
	Def <=(a: Int,    b: Long)  : Bool = ;
	Def <=(a: Long,   b: Char)  : Bool = ;
	Def <=(a: Char,   b: Long)  : Bool = ;

	Def >(a: Long,   b: Long)  : Bool = ;
	Def >(a: Long,   b: Double): Bool = ;
	Def >(a: Double, b: Long)  : Bool = ;
	Def >(a: Long,   b: Float) : Bool = ;
	Def >(a: Float,  b: Long)  : Bool = ;
	Def >(a: Long,   b: Int)   : Bool = ;
	Def >(a: Int,    b: Long)  : Bool = ;
	Def >(a: Long,   b: Char)  : Bool = ;
	Def >(a: Char,   b: Long)  : Bool = ;

	Def >=(a: Long,   b: Long)  : Bool = ;
	Def >=(a: Long,   b: Double): Bool = ;
	Def >=(a: Double, b: Long)  : Bool = ;
	Def >=(a: Long,   b: Float) : Bool = ;
	Def >=(a: Float,  b: Long)  : Bool = ;
	Def >=(a: Long,   b: Int)   : Bool = ;
	Def >=(a: Int,    b: Long)  : Bool = ;
	Def >=(a: Long,   b: Char)  : Bool = ;
	Def >=(a: Char,   b: Long)  : Bool = ;

	// Equals operators

	Def ==(a: Long,   b: Long)  : Bool = ;
	Def ==(a: Long,   b: Double): Bool = ;
	Def ==(a: Double, b: Long)  : Bool = ;
	Def ==(a: Long,   b: Float) : Bool = ;
	Def ==(a: Float,  b: Long)  : Bool = ;
	Def ==(a: Long,   b: Int)   : Bool = ;
	Def ==(a: Int,    b: Long)  : Bool = ;
	Def ==(a: Long,   b: Char)  : Bool = ;
	Def ==(a: Char,   b: Long)  : Bool = ;

	Def !=(a: Long,   b: Long)  : Bool = ;
	Def !=(a: Long,   b: Double): Bool = ;
	Def !=(a: Double, b: Long)  : Bool = ;
	Def !=(a: Long,   b: Float) : Bool = ;
	Def !=(a: Float,  b: Long)  : Bool = ;
	Def !=(a: Long,   b: Int)   : Bool = ;
	Def !=(a: Int,    b: Long)  : Bool = ;
	Def !=(a: Long,   b: Char)  : Bool = ;
	Def !=(a: Char,   b: Long)  : Bool = ;

	// Unary operators
	Def #(a: Long) : Int  = ;
	Def -(a: Long) : Long = ;
	Def ~(a: Long) : Long = ;
	Def ++(a: Long): Long = ;
	Def --(a: Long): Long = ;
