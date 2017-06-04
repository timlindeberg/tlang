package T::lang

class CharRef =

	var v: Char

	Def static ValueOf(v: Char) = new CharRef(v)
	Def Value() = v

	Def toString() = java::lang::String.valueOf(v)
	def new(v: Char) = (this.v = v)
