package T::lang

class CharRef =

	val v: Char

	Def static ValueOf(v: Char): CharRef = new CharRef(v)
	Def Value(): Char = v

	Def toString(): String = java::lang::String.valueOf(v)
	def new(v: Char) = (this.v = v)
