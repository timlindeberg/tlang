package T::lang

extension T::lang::Float =

	Def static MaxExponent(): Int = java::lang::Float.MAX_EXPONENT
	Def static MaxValue(): Float  = java::lang::Float.MAX_VALUE

	Def static MinExponent(): Int = java::lang::Float.MIN_EXPONENT
	Def static MinValue(): Float  = java::lang::Float.MIN_VALUE
	Def static MinNormal(): Float = java::lang::Float.MIN_NORMAL

	Def static Size(): Int  = java::lang::Float.SIZE
	Def static Bytes(): Int = java::lang::Float.BYTES

	Def static NaN(): Float              = java::lang::Float.NaN
	Def static PositiveInfinity(): Float = java::lang::Float.POSITIVE_INFINITY
	Def static NegativeInfinity(): Float = java::lang::Float.NEGATIVE_INFINITY

	Def static Parse(s: String): Float = java::lang::Float.parseFloat(s)

	Def ToIntBits(): Int    = java::lang::Float.floatToIntBits(this)
	Def ToRawIntBits(): Int = java::lang::Float.floatToRawIntBits(this)
	Def IsInfinite(): Bool  = java::lang::Float.isInfinite(this)
	Def IsFinite(): Bool    = java::lang::Float.isFinite(this)
	Def IsNaN(): Bool       = java::lang::Float.isNaN(this)

	Def ToHexString(): String = java::lang::Float.toHexString(this)

	Def toString(): String    = java::lang::Float.toString(this)
