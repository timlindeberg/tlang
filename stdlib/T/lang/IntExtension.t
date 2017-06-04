package T::lang

extension T::lang::Int =

	Def static MaxValue(): Int = java::lang::Integer.MAX_VALUE
	Def static MinValue(): Int = java::lang::Integer.MIN_VALUE
	Def static Size(): Int     = java::lang::Integer.SIZE
	Def static Bytes(): Int    = java::lang::Integer.BYTES

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

	Def toString(): String = java::lang::Integer.toString(this)

	Def [](index: Int): Int =
		if(index > 31 || index < 0)
			error("Index out of bounds: " + index)

		(this & (1 << index)) != 0 ? 1 : 0
