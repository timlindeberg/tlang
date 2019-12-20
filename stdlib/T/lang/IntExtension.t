package T::lang

extension IntExtension: Int =

	Def static MaxValue(): Int = java::lang::Integer.MAX_VALUE
	Def static MinValue(): Int = java::lang::Integer.MIN_VALUE
	Def static Size(): Int     = java::lang::Integer.SIZE
	Def static Bytes(): Int    = java::lang::Integer.BYTES

	Def static Parse(s: String): Int             = java::lang::Integer.parseInt(s)
	Def static Parse(s: String, radix: Int): Int = java::lang::Integer.parseInt(s, radix)

	Def BitsToFloat(): Float            = java::lang::Float.intBitsToFloat(this)
	Def BitCount(): Int                 = java::lang::Integer.bitCount(this)
	Def HighestOneBit(): Int            = java::lang::Integer.highestOneBit(this)
	Def LowestOneBit(): Int             = java::lang::Integer.lowestOneBit(this)
	Def NumberOfLeadingZeros(): Int     = java::lang::Integer.numberOfLeadingZeros(this)
	Def NumberOfTrailingZeros(): Int    = java::lang::Integer.numberOfTrailingZeros(this)
	Def Reverse(): Int                  = java::lang::Integer.reverse(this)
	Def ReverseBytes(): Int             = java::lang::Integer.reverseBytes(this)
	Def RotateLeft(distance: Int): Int  = java::lang::Integer.rotateLeft(this, distance)
	Def RotateRight(distance: Int): Int = java::lang::Integer.rotateRight(this, distance)
	Def Sign(): Int                     = java::lang::Integer.signum(this)
	Def ToBinaryString(): String        = java::lang::Integer.toBinaryString(this)
	Def ToHexString(): String           = java::lang::Integer.toHexString(this)
	Def ToOctalString(): String         = java::lang::Integer.toOctalString(this)

	Def SetBit(bit: Int): Int    = this | (1 << bit)
	Def ClearBit(bit: Int): Int  = this & ~(1 << bit)
	Def ToggleBit(bit: Int): Int = this ^ (1 << bit)

	Def toString(): String = java::lang::Integer.toString(this)

	Def [](index: Int): Int =
		if(index > 31 || index < 0)
			error("Index out of bounds: " + index)

		(this & (1 << index)) != 0 ? 1 : 0

	Def [:](start: Int?, end: Int?, step: Int?): Int =
		val s = start ?: 0
		val e = end ?: 31
		if(step != null)
			error("Int slicing does not support stepping: " + step)

		if(s > 31 || s < 0)
			error("Index out of bounds: " + s)

		if(e > 31 || e < 0)
			error("Index out of bounds: " + e)

		if(s > e)
			error("Starting index can't be greater or equal to ending index: " + s + " > " + e)

		val allSet = 0xFFFFFFFF
		val mask = (allSet >> s) & (allSet << (31 - e))
		this & mask

