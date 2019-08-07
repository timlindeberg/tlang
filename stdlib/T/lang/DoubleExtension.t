package T::lang

extension DoubleExtension: Double =

	Def static MaxExponent(): Int  = java::lang::Double.MAX_EXPONENT
	Def static MaxValue(): Double  = java::lang::Double.MAX_VALUE

	Def static MinExponent(): Int  = java::lang::Double.MIN_EXPONENT
	Def static MinValue(): Double  = java::lang::Double.MIN_VALUE
	Def static MinNormal(): Double = java::lang::Double.MIN_NORMAL

	Def static Size(): Int  = java::lang::Double.SIZE
	Def static Bytes(): Int = java::lang::Double.BYTES

	Def static NaN(): Double              = java::lang::Double.NaN
	Def static PositiveInfinity(): Double = java::lang::Double.POSITIVE_INFINITY
	Def static NegativeInfinity(): Double = java::lang::Double.NEGATIVE_INFINITY

	Def static Parse(s: String): Double = java::lang::Double.parseDouble(s)

	Def ToLongBits(): Long    = java::lang::Double.doubleToLongBits(this)
	Def ToRawLongBits(): Long = java::lang::Double.doubleToRawLongBits(this)
	Def IsInfinite(): Bool    = java::lang::Double.isInfinite(this)
	Def IsFinite(): Bool      = java::lang::Double.isFinite(this)
	Def IsNaN(): Bool         = java::lang::Double.isNaN(this)

	Def ToHexString(): String = java::lang::Double.toHexString(this)

	Def toString(): String    = java::lang::Double.toString(this)
