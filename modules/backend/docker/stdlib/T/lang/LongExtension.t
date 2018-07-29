package T::lang

extension T::lang::Long =

	Def static MaxValue(): Long = java::lang::Long.MAX_VALUE
	Def static MinValue(): Long = java::lang::Long.MIN_VALUE
	Def static Size(): Int      = java::lang::Long.SIZE
	Def static Bytes(): Int     = java::lang::Long.BYTES

	Def static Parse(s: String): Long             = java::lang::Long.parseLong(s)
	Def static Parse(s: String, radix: Int): Long = java::lang::Long.parseLong(s, radix)


	Def BitsToDouble(): Double           = java::lang::Double.longBitsToDouble(this)
	Def BitCount(): Long                 = java::lang::Long.bitCount(this)
	Def HighestOneBit(): Long            = java::lang::Long.highestOneBit(this)
	Def LowestOneBit(): Long             = java::lang::Long.lowestOneBit(this)
	Def NumberOfLeadingZeros(): Int      = java::lang::Long.numberOfLeadingZeros(this)
	Def NumberOfTrailingZeros(): Int     = java::lang::Long.numberOfTrailingZeros(this)
	Def Reverse(): Long                  = java::lang::Long.reverse(this)
	Def ReverseBytes(): Long             = java::lang::Long.reverseBytes(this)
	Def RotateLeft(distance: Int): Long  = java::lang::Long.rotateLeft(this, distance)
	Def RotateRight(distance: Int): Long = java::lang::Long.rotateRight(this, distance)
	Def Sign(): Long                     = java::lang::Long.signum(this)
	Def ToBinaryString(): String         = java::lang::Long.toBinaryString(this)
	Def ToHexString(): String            = java::lang::Long.toHexString(this)
	Def ToOctalString(): String          = java::lang::Long.toOctalString(this)

	Def toString(): String = java::lang::Long.toString(this)

	Def [](index: Int): Int =
		if(index > 63 || index < 0)
			error("Index out of bounds: " + index)

		(this & (1L << index)) != 0 ? 1 : 0

	Def [::](start: Int?, end: Int?, step: Int?): Long =
		val s = start ?: 0
		val e = end ?: 63
		if(step != null)
			error("Long slicing does not support stepping: " + step)

		if(s > 63 || s < 0)
			error("Index out of bounds: " + s)

		if(e > 63 || e < 0)
			error("Index out of bounds: " + e)

		if(s > e)
			error("Starting index can't be greater or equal to ending index: " + s + " > " + e)

		val allSet = 0xFFFFFFFFFFFFFFFFL
		val mask = (allSet >> s) & (allSet << (63 - e))
		this & mask
